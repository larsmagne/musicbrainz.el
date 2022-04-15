;;; musicbrainz.el --- musicbrainz interface
;; Copyright (C) 2011 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; This file is not part of GNU Emacs.

;; musicbrainz.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; musicbrainz.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This library queries the musicbrainz.org data base and converts the
;; entries into cddb form.

;;; Code:

(require 'cl)
(require 'message)
(require 'url)
(require 'mm-url)

(defvar musicbrainz-cdrom "/dev/scd0"
  "The device name to be given to the cd-discid program.")

(defun musicbrainz-query (discid)
  (let ((buffer
	 (url-retrieve-synchronously
	  (format "http://musicbrainz.org/ws/1/release/?discid=%s&type=xml"
		  discid))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (let ((xml (xml-parse-region (point) (point-max))))
	    (kill-buffer buffer)
	    (and (cdr (nth 2 (nth 2 (car xml))))
		 xml)))))))

(defun musicbrainz-search (artist title)
  (let ((buffer
	 (url-retrieve-synchronously
	  (format "http://musicbrainz.org/ws/1/release/?artist=%s&title=%s&type=xml&inc=tracks"
		  artist title))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (xml-parse-region (point) (point-max)))))))

(defun musicbrainz-to-cddb (xml)
  (let* ((release (cdr (nth 2 (nth 2 (car xml)))))
	 (date (cdr
		(assq 'date (cadr
			     (nth 2 (assq 'release-event-list release))))))
	 track-names frames
	 (start-frame 150))
    (when release
      (dolist (track (nthcdr 2 (assq 'track-list release)))
	(push (decode-coding-string (nth 2 (assq 'title track)) 'utf-8)
	      track-names)
	(push start-frame frames)
	(setq start-frame
	      (+ start-frame
		 (round
		  (/ (string-to-number (or (nth 2 (assq 'duration track)) "0"))
		     13.3333)))))
      (list
       (cons 'id "")
       (cons 'frames (nreverse frames))
       (cons 'tracks (nreverse track-names))
       (cons 'artist (decode-coding-string
		      (or (nth 2 (assq 'name (cdr (assq 'artist release)))) "")
		      'utf-8))
       (cons 'title (decode-coding-string
		     (nth 2 (assq 'title release))
		     'utf-8))
       (cons 'length (/ start-frame 75))
       (cons 'year (and date
			(substring date 0 4)))))))

(defun musicbrainz-toc (&optional cdrom)
  "Get the Table Of Contents by using the cd-discid extenal command."
  (let ((output (shell-command-to-string
		 (format "cd-discid --musicbrainz %s"
			 (or cdrom musicbrainz-cdrom)))))
    (if (not output)
	(error "No output from cd-discid")
      (let ((toc (mapcar #'string-to-number (split-string output))))
	;; Rejuggle the order, so that it corresponds to what we're
	;; expecting.
	(setcdr toc (cons (car (last toc)) (cdr toc)))
	(setcdr (last toc 2) nil)
	(push 1 toc)
	(list (cons 'toc toc)
	      (cons 'id (musicbrainz-discid toc)))))))

(defun musicbrainz-discid (toc)
  (with-temp-buffer
    (insert (format "%02X%02X" (pop toc) (pop toc)))
    (dotimes (i 100)
      (insert (format "%08X" (or (pop toc) 0))))
    (subst-char-in-string
     ?+ ?.
     (subst-char-in-string
      ?/ ?_
      (subst-char-in-string
       ?= ?-
       (base64-encode-string (sha1 (buffer-string) nil nil t)))))))

(defun musicbrainz-possibly-submit (toc cddb-entry)
  "If TOC doesn't exist on MusicBrains, then submit CDDB-ENTRY."
  (unless (musicbrainz-query (cdr (assq 'id toc)))
    (musicbrainz-submit toc cddb-entry)))

(defun musicbrainz-submit (toc cddb-entry)
  (let ((url (format
	      "http://musicbrainz.org/ws/1/release/?client=musicbrainz.el-1.0&title=%s&toc=%s&discid=%s"
	      (musicbrainz-encode
	       (cdr (assq 'title cddb-entry)))
	      (musicbrainz-encode
	       (mapconcat #'number-to-string (cdr (assq 'toc toc)) " "))
	      (cdr (assq 'id toc))))
	(artist (cdr (assq 'artist cddb-entry)))
	(i 0)
	elem)
    (unless (equal artist "Various")
      (setq url (concat url "&artist=" (musicbrainz-encode artist))))
    (dolist (track (cdr (assq 'tracks cddb-entry)))
      (setq url
	    (concat
	     url
	     (if (and (equal artist "Various")
		      (> (length (setq elem (split-string track " - "))) 1))
		 (format "&track%d=%s&artist%d=%s"
			 i (musicbrainz-encode (cadr elem))
			 i (musicbrainz-encode (car elem)))
	       (format "&track%d=%s"
		       i (musicbrainz-encode track)))))
      (incf i))
    (let* ((url-request-method "POST")
	   (url-request-extra-headers '(("Content-length" . "0")))
	   (buffer (url-retrieve-synchronously url)))
      (with-current-buffer buffer
	(prog1
	    (buffer-string)
	  (kill-buffer buffer))))))

(defun musicbrainz-encode (string)
  (mm-url-form-encode-xwfu (encode-coding-string string 'utf-8)))

(provide 'musicbrainz)

;;; musicbrainz.el ends here
