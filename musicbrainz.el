;;; musicbrainz.el --- musicbrainz interface
;; Copyright (C) 1998, 2002, 2011 Lars Magne Ingebrigtsen

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

;; This library queries the musicbrainz.org data base and coverts the
;; entries into cddb form.  Submission hasn't been implemented (yet).

;;; Code:

(require 'cl)
(require 'message)
(require 'url)

(defun musicbrainz-query (discid)
  (let ((buffer
	 (url-retrieve-synchronously
	  (format "http://musicbrainz.org/ws/1/release/?discid=%s&type=xml"
		  discid))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(goto-char (point-min))
	(when (search-forward "\n\n" nil t)
	  (xml-parse-region (point) (point-max)))))))

(defun musicbrainz-to-cddb (xml)
  (let ((release (cdr (nth 2 (nth 2 (car xml)))))
	track-names frames
	(start-frame 150))
    (dolist (track (nthcdr 2 (assq 'track-list release)))
      (push (nth 2 (assq 'title track)) track-names)
      (push start-frame frames)
      (setq start-frame
	    (+ start-frame
	       (round
		(/ (string-to-number (nth 2 (assq 'duration track)))
		   13.3333)))))
    (list
     (cons 'frames (nreverse frames))
     (cons 'tracks (nreverse track-names))
     (cons 'artist (nth 2 (assq 'name (cdr (assq 'artist release)))))
     (cons 'title (nth 2 (assq 'title release)))
     (cons 'length (/ start-frame 75))
     (cons 'year (substring
		  (cdr
		   (assq 'date (cadr
				(nth 2 (assq 'release-event-list release)))))
		  0 4)))))

(provide 'musicbrainz)

;;; musicbrainz.el ends here
