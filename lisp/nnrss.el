;;; nnrss.el --- interfacing with RSS
;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: RSS

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'nnoo)
(require 'nnmail)
(require 'message)
(require 'mm-util)
(require 'gnus-util)
(require 'time-date)
(require 'rfc2231)
(eval-when-compile
  (ignore-errors
    (require 'xml)
    (require 'w3)
    (require 'w3-forms)
    (require 'nnweb)))
;; Report failure to find w3 at load time if appropriate.
(eval '(progn
	 (require 'xml)
	 (require 'w3)
	 (require 'w3-forms)
	 (require 'nnweb)))

(nnoo-declare nnrss)

(defvoo nnrss-directory (nnheader-concat gnus-directory "rss/")
  "Where nnrss will save its files.")

;; (group max rss-url)
(defvoo nnrss-server-data nil)

;; (num timestamp url subject author date extra)
(defvoo nnrss-group-data nil)
(defvoo nnrss-group-max 0)
(defvoo nnrss-group-min 1)
(defvoo nnrss-group nil)
(defvoo nnrss-group-hashtb nil)
(defvoo nnrss-status-string "")

(defconst nnrss-version "nnrss 1.0")

(defvar nnrss-group-alist
  '(("MacWeek"
     "http://macweek.zdnet.com/macweek.xml"
     "The Macintosh news authority.")
    ("Linux.Weekly.News"
     "http://lwn.net/headlines/rss")
    ("Motley.Fool"
     "http://www.fool.com/About/headlines/rss_headlines.asp")
    ("NewsForge.rdf"
     "http://www.newsforge.com/newsforge.rdf")
    ("Slashdot"
     "http://www.slashdot.com/slashdot.rdf")
    ("CNN"
     "http://www.cnn.com/cnn.rss"
     "The world's news leader.")
    ("FreshMeat"
     "http://freshmeat.net/backend/fm.rdf"
     "The one-stop-shop for all your Linux software needs.")
    ("The.Guardian.newspaper"
     "http://www.guardianunlimited.co.uk/rss/1,,,00.xml"
     "Intelligent news and comment throughout the day from The Guardian newspaper.")
    ("MonkeyFist.rdf"
     "http://monkeyfist.com/rdf.php3"
     "News and opinion on politics, technology, and eclectic miscellany.")
    ("NewsForge"
     "http://www.newsforge.com/newsforge.rss")
    ("Reuters.Health"
     "http://www.reutershealth.com/eline.rss"
     "Consumer-oriented health-related news stories.")
    ("Salon"
     "http://www.salon.com/feed/RDF/salon_use.rdf")
    ("Wired"
     "http://www.wired.com/news_drop/netcenter/netcenter.rdf")
    ("ITN"
     "http://www.itn.co.uk/itn.rdf")
    ("Meerkat"
     "http://www.oreillynet.com/meerkat/?_fl=rss10"
     "An Open Wire Service")
    ("MonkeyFist"
     "http://monkeyfist.com/rss1.php3"
     "News and opinion on politics, technology, and eclectic miscellany.")
    ("Reuters.Health.rdf"
     "http://www.reutershealth.com/eline.rdf"
     "Consumer-oriented health-related news stories.")
    ;;("4xt" "http://4xt.org/news/general.rss10" "Resources for XT users.")
    ("Aaronland" "http://aaronland.net/xml/abhb.rdf" "A boy and his basement.")
    ("Art of the Mix" "http://www.artofthemix.org/xml/rss.asp" "A website devoted to the art of making mixed tapes and cds.")
    ("Dave Beckett's RDF Resource Guide" "http://www.ilrt.bristol.ac.uk/discovery/rdf/resources/rss.rdf" "A comprehensive guide to resources about RDF.")
    ("David Chess" "http://www.davidchess.com/words/log.rss" "Mostly-daily musings on philosophy, children, culture, technology, the emergence of life from matter, chocolate, Nomic, and all that sort of thing.")
    ;;("Dublin Core Metadata Intitiative" "http://www.dublincore.org/news.rss" "Latest news from DCMI.")
    ("Figby Articles" "http://www.figby.com/index-rss.php" "A weblog with daily stories about technology, books and publishing, privacy, science, and occasional humor.")
    ;;("Figby News" "http://www.figby.com/news.php" "Categorized RSS feeds from various sources.")
    ("Figby Quickies" "http://www.figby.com/quickies-rss.php" "Quick commented links to other sites from Figby.com.")
    ("Flutterby!" "http://www.flutterby.com/main.rdf" "News and views from Dan Lyke.")
    ("Groovelog" "http://groovelog.agora.co.uk/groove+log/groovelog.nsf/today.rss.xml" "The open-access groove users' weblog.")
    ;;("Groovelog.rss10" "http://groovelog.agora.co.uk/groove+log/groovelog.nsf/today.rss10.xml" "The open-access groove users' weblog.")
    ("Hit or Miss" "http://hit-or-miss.org/rss/" "Daily weblog and journal.")
    ;;("Internet.com Feeds" "http://www.webreference.com/services/news/" "News from ")
    ("Larkfarm News" "http://www.larkfarm.com/Larkfarm.rdf" "Mike Gunderloy's web site.")
    ("Latest RFCs" "http://x42.com/rss/rfc.rss")
    ("Linux Today" "http://linuxtoday.com/backend/biglt.rss")
    ("Linux Today.rdf" "http://linuxtoday.com/backend/my-netscape10.rdf")
    ("More Like This WebLog" "http://www.whump.com/moreLikeThis/RSS" "Because the more you know, the more jokes you get.")
    ("Motivational Quotes of the Day" "http://www.quotationspage.com/data/mqotd.rss" "Four motivational quotations each day from the Quotations Page.")
    ;;("My Netscape Network" "http://www.dmoz.org/Netscape/My_Netscape_Network/")
    ;;("My UserLand" "http://my.userland.com/choose")
    ("Network World Fusion NetFlash" "http://www.nwfusion.com/netflash.rss" "Daily breaking news about networking products, technologies and services.")
    ;;("News Feeds" "http://newsfeeds.manilasites.com/" "Jeff Barr highlights high quality RSS feeds.")
    ;;("News Is Free Export" "http://www.newsisfree.com/export.php3")
    ("News Is Free" "http://www.newsisfree.com/news.rdf.php3")
    ;;("News is Free XML Export" "http://www.newsisfree.com/ocs/directory.xml")
    ("O'Reilly Network Articles" "http://www.oreillynet.com/cs/rss/query/q/260?x-ver=1.0")
    ("Quotes of the Day" "http://www.quotationspage.com/data/qotd.rss" "Four humorous quotations each day from the Quotations Page.")
    ("RDF Interest Group" "http://ilrt.org/discovery/rdf-dev/roads/cgi-bin/desire/ig2rss?list=www-rdf-interest" "An experimental channel scraped from the RDF Interest Group mail archives.")
    ("RDF Logic List" "http://ilrt.org/discovery/rdf-dev/roads/cgi-bin/desire/ig2rss?list=www-rdf-logic" "An experimental channel scraped from the RDF Logic mail archives.")
    ("RSS Info" "http://www.blogspace.com/rss/rss10" "News and information on the RSS format")
    ;;("RSS-DEV listing" "http://www.egroups.com/links/rss-dev/Feeds_000966335046/" "A listing of RSS files from the RSS-DEV list.")
    ("Semantic Web List" "http://ilrt.org/discovery/rdf-dev/roads/cgi-bin/desire/ig2rss?list=semantic-web" "An experimental channel scraped from the W3C's Semantic Web mail archives.")
    ;;("Sherch!" "http://www.sherch.com/~pldms/cgi-bin/sherch.pl" "Sherlock for the rest of us.")
    ;;("Street Fusion Archived Financial Webcasts" "http://partners.streetfusion.com/rdf/archive.rdf")
    ;;("Street Fusion Upcoming Financial Webcasts" "http://partners.streetfusion.com/rdf/live.rdf")
    ;;("TNL.net newsletter" "http://www.tnl.net/newsletter/channel100.asp" "A newsletter about Internet technology and issues.")
    ("W3C" "http://www.w3.org/2000/08/w3c-synd/home.rss" "The latest news at the World Wide Web Consortium.")
    ;;("XML News: RSS Live Content" "http://www.xmlnews.org/RSS/content.html" "A listing of well-known RSS feeds.")
    ("|fr| XMLfr" "http://xmlfr.org/actualites/general.rss10" 
     "French speaking portal site dedicated to XML.")
    ("XMLhack" "http://xmlhack.com/rss10.php" 
     "Developer news from the XML community.")
    ("The Register" 
     "http://www.theregister.co.uk/tonys/slashdot.rdf" 
     "The Register -- Biting the hand that feeds IT.")
    ("|de| Heise-Ticker" 
     "http://www.heise.de/newsticker/heise.rdf" 
     "German news ticker about technology.")
    ("|de| Telepolis News" 
     "http://www.heise.de/tp/news.rdf" 
     "German background news about technology.")
    ("Kuro5hin" 
     "http://www.kuro5hin.org/backend.rdf"
     "Technology and culture, from the trenches.")
    ("JabberCentral"
     "http://www.jabbercentral.com/rss.php"
     "News around the Jabber instant messaging system.")))

(defvar nnrss-use-local nil)

(defvar nnrss-description-field 'X-Gnus-Description
  "Field name used for DESCRIPTION.
To use the description in headers, put this name into `nnmail-extra-headers'.")

(nnoo-define-basics nnrss)

;;; Interface functions

(deffoo nnrss-retrieve-headers (articles &optional group server fetch-old)
  (nnrss-possibly-change-group group server)
  (let (e)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (dolist (article articles)
	(if (setq e (assq article nnrss-group-data))
	    (insert (number-to-string (car e)) "\t" ;; number
		    (if (nth 3 e)
			(nnrss-format-string (nth 3 e)) "")
		    "\t" ;; subject
		    (if (nth 4 e)
			(nnrss-format-string (nth 4 e))
		      "(nobody)")
		    "\t" ;;from
		    (or (nth 5 e) "")
		    "\t" ;; date
		    (format "<%d@%s.nnrss>" (car e) group)
		    "\t" ;; id
		    "\t" ;; refs
		    "0" "\t" ;; chars
		    "0" "\t" ;; lines
		    "" "\t" ;; Xref
		    (if (memq nnrss-description-field nnmail-extra-headers)
			(concat (symbol-name nnrss-description-field)
				": "
				(nnrss-format-string (nth 6 e)) "\t")
		      "")
		    "\n")))))
  'nov)

(deffoo nnrss-request-group (group &optional server dont-check)
  (nnrss-possibly-change-group group server)
  (if dont-check
      t
    (nnrss-check-group group server)
    (nnheader-report 'nnrss "Opened group %s" group)
    (nnheader-insert
     "211 %d %d %d %s\n" nnrss-group-max nnrss-group-min nnrss-group-max
     (prin1-to-string group)
     t)))

(deffoo nnrss-close-group (group &optional server)
  t)

(deffoo nnrss-request-article (article &optional group server buffer)
  (nnrss-possibly-change-group group server)
  (let ((e (assq article nnrss-group-data))
	(nntp-server-buffer (or buffer nntp-server-buffer))
	post err)
    (when e
      (catch 'error
	(with-current-buffer nntp-server-buffer
	  (erase-buffer)
	  (goto-char (point-min))
	  (if (nth 3 e)
	      (insert "Subject: " (nnrss-format-string (nth 3 e)) "\n"))
	  (if (nth 4 e)
	      (insert "From: " (nnrss-format-string (nth 4 e)) "\n"))
	  (if (nth 5 e)
	      (insert "Date: " (nnrss-format-string (nth 5 e)) "\n"))
	  (insert "Message-ID: " (format "<%d@%s.nnrss>" (car e) group) "\n")
	  (insert "\n")
	  (if (nth 6 e)
	      (let ((point (point)))
		(insert (nnrss-string-as-multibyte (nth 6 e)) "\n\n")
		(fill-region point (point))))
	  (if (nth 2 e)
	      (insert (nth 2 e) "\n")))))
    (cond
     (err
      (nnheader-report 'nnrss err))
     ((not e)
      (nnheader-report 'nnrss "No such id: %d" article))
     (t
      (nnheader-report 'nnrss "Article %s retrieved" (car e))
      ;; We return the article number.
      (cons nnrss-group (car e))))))

(deffoo nnrss-request-list (&optional server)
  (nnrss-possibly-change-group nil server)
  (nnrss-generate-active)
  t)

(deffoo nnrss-open-server (server &optional defs connectionless)
  (nnrss-read-server-data server)
  (nnoo-change-server 'nnrss server defs)
  t)

(deffoo nnrss-request-expire-articles
    (articles group &optional server force)
  (nnrss-possibly-change-group group server)
  (let (e days not-expirable changed)
    (dolist (art articles)
      (if (and (setq e (assq art nnrss-group-data))
	       (nnmail-expired-article-p
		group
		(if (listp (setq days (nth 1 e))) days 
		  (days-to-time (- days (time-to-days '(0 0)))))
		force))
	  (setq nnrss-group-data (delq e nnrss-group-data)
		changed t)
	(push art not-expirable)))
    (if changed
	(nnrss-save-group-data group server))
    not-expirable))

(deffoo nnrss-request-delete-group (group &optional force server)
  (nnrss-possibly-change-group group server)
  (setq nnrss-server-data
	(delq (assoc group nnrss-server-data) nnrss-server-data))
  (nnrss-save-server-data server)
  (let ((file (expand-file-name 
	       (nnrss-translate-file-chars
		(concat group (and server
				   (not (equal server ""))
				   "-")
			server ".el")) nnrss-directory)))
    (ignore-errors
      (delete-file file)))
  t)

(deffoo nnrss-request-list-newsgroups (&optional server)
  (nnrss-possibly-change-group nil server)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (dolist (elem nnrss-group-alist)
      (if (third elem)
	  (insert (car elem) "\t" (third elem) "\n"))))
  t)

(nnoo-define-skeleton nnrss)

;;; Internal functions

(defun nnrss-possibly-change-group (&optional group server)
  (when (and server
	     (not (nnrss-server-opened server)))
    (nnrss-open-server server))
  (when (and group (not (equal group nnrss-group)))
    (nnrss-read-group-data group server)
    (setq nnrss-group group)))

(defvar nnrss-extra-categories '(nnrss-snarf-moreover-categories))

(defun nnrss-generate-active ()
  (if (y-or-n-p "Fetch extra categories? ")
      (dolist (func nnrss-extra-categories)
	(funcall func)))
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (dolist (elem nnrss-group-alist)
      (insert (prin1-to-string (car elem)) " 0 1 y\n"))
    (dolist (elem nnrss-server-data)
      (unless (assoc (car elem) nnrss-group-alist)
	(insert (prin1-to-string (car elem)) " 0 1 y\n")))))

;;; Data functions

(defun nnrss-read-server-data (server)
  (setq nnrss-server-data nil)
  (let ((file (expand-file-name 
	       (nnrss-translate-file-chars
		(concat "nnrss" (and server
				     (not (equal server ""))
				     "-")
			server
			".el"))
	       nnrss-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
	(let ((coding-system-for-read 'binary))
	  (insert-file-contents file))
	(emacs-lisp-mode)
	(goto-char (point-min))
	(eval-buffer)))))

(defun nnrss-save-server-data (server)
  (gnus-make-directory nnrss-directory)
  (let ((file (expand-file-name 
	       (nnrss-translate-file-chars
		(concat "nnrss" (and server
				     (not (equal server ""))
				     "-")
			server ".el"))
	       nnrss-directory)))
    (let ((coding-system-for-write 'binary))
      (with-temp-file file
	(insert "(setq nnrss-server-data '"
		(prin1-to-string nnrss-server-data)
		")\n")))))

(defun nnrss-read-group-data (group server)
  (setq nnrss-group-data nil)
  (setq nnrss-group-hashtb (gnus-make-hashtable))
  (let ((pair (assoc group nnrss-server-data)))
    (setq nnrss-group-max (or (cadr pair) 0))
    (setq nnrss-group-min (+ nnrss-group-max 1)))
  (let ((file (expand-file-name 
	       (nnrss-translate-file-chars
		(concat group (and server
				   (not (equal server ""))
				   "-")
			server ".el"))
	       nnrss-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
	(let ((coding-system-for-read 'binary))
	  (insert-file-contents file))
	(emacs-lisp-mode)
	(goto-char (point-min))
	(eval-buffer))
      (dolist (e nnrss-group-data)
	(gnus-sethash (nth 2 e) e nnrss-group-hashtb)
	(if (and (car e) (> nnrss-group-min (car e)))
	    (setq nnrss-group-min (car e)))
	(if (and (car e) (< nnrss-group-max (car e)))
	    (setq nnrss-group-max (car e)))))))

(defun nnrss-save-group-data (group server)
  (gnus-make-directory nnrss-directory)
  (let ((file (expand-file-name 
	       (nnrss-translate-file-chars
		(concat group (and server
				   (not (equal server ""))
				   "-")
			server ".el"))
	       nnrss-directory)))
    (let ((coding-system-for-write 'binary))
      (with-temp-file file
	(insert "(setq nnrss-group-data '"
		(prin1-to-string nnrss-group-data)
		")\n")))))

;;; URL interface

(defun nnrss-no-cache (url)
  "")

;; TODO:: disable cache.
;;
;; (defun nnrss-insert-w3 (url)
;;   (require 'url)
;;   (require 'url-cache)
;;   (let ((url-cache-creation-function 'nnrss-no-cache))
;;     (mm-with-unibyte-current-buffer
;;       (nnweb-insert url))))

(defun nnrss-insert-w3 (url)
  (mm-with-unibyte-current-buffer
    (nnweb-insert url)))

(defun nnrss-decode-entities-unibyte-string (string)
  (mm-with-unibyte-buffer
    (insert string)
    (nnweb-decode-entities)
    (buffer-substring (point-min) (point-max))))

(defalias 'nnrss-insert 'nnrss-insert-w3)

(if (featurep 'xemacs)
    (defalias 'nnrss-string-as-multibyte 'identity)
  (defalias 'nnrss-string-as-multibyte 'string-as-multibyte))

;;; Snarf functions

(defun nnrss-check-group (group server)
  (let ((w3-html-entities (cons '(nbsp . 32) w3-html-entities))
	file xml subject url extra changed author date)
    (condition-case err
	(mm-with-unibyte-buffer
	  (if (and nnrss-use-local
		   (file-exists-p (setq file (expand-file-name
					  (nnrss-translate-file-chars
					   (concat group ".xml"))
					  nnrss-directory))))
	      (insert-file-contents file)
	    (setq url (or (nth 2 (assoc group nnrss-server-data))
			  (second (assoc group nnrss-group-alist))))
	    (unless url
	      (setq url
		(read-string (format "RSS url of %s: " group "http://")))
	      (let ((pair (assoc group nnrss-server-data)))
		(if pair
		    (setcdr (cdr pair) (list url))
		  (push (list group nnrss-group-max url) nnrss-server-data)))
	      (setq changed t))
	    (nnrss-insert url))
	  (goto-char (point-min))
	  (while (re-search-forward "\r\n?" nil t)
	    (replace-match "\n"))
	  (goto-char (point-min))
	  (if (re-search-forward "<rdf\\|<rss" nil t)
	      (goto-char (match-beginning 0)))
	  (setq xml (xml-parse-region (point) (point-max))))
      (error 
       (nnheader-message 1 "Error in group %s: %s" group (cadr err))))
    (while (and xml (not (assq 'item xml)))
      (unless (listp (car (setq xml (cddar xml))))
	(setq xml nil)))
    (dolist (item (nreverse xml))
       (when (and (listp item)
		  (eq 'item (car item))
		  (setq url (nnrss-node-text (assq 'link (cddr item))))
		  (setq url (nnrss-decode-entities-unibyte-string url))
		  (not (gnus-gethash url nnrss-group-hashtb)))
	 (setq subject (nnrss-node-text (assq 'title (cddr item))))
	 (setq extra (or (nnrss-node-text (assq 'description (cddr item)))
			 (nnrss-node-text (assq 'dc:description (cddr item)))))
	 (setq author (nnrss-node-text (assq 'dc:creator (cddr item))))
	 (setq date (or (nnrss-node-text (assq 'dc:date (cddr item)))
			(message-make-date)))
	 (push
	  (list
	   (incf nnrss-group-max)
	   (current-time)
	   url
	   (and subject (nnrss-decode-entities-unibyte-string subject))
	   (and author (nnrss-decode-entities-unibyte-string author))
	   date
	   (and extra (nnrss-decode-entities-unibyte-string extra)))
	  nnrss-group-data)
	 (gnus-sethash url (car nnrss-group-data) nnrss-group-hashtb)
	 (setq changed t)))
    (when changed
      (nnrss-save-group-data group server)
      (let ((pair (assoc group nnrss-server-data)))
	(if pair
	    (setcar (cdr pair) nnrss-group-max)
	  (push (list group nnrss-group-max) nnrss-server-data)))
      (nnrss-save-server-data server))))

(defun nnrss-generate-download-script ()
  "Generate a download script in the current buffer.
It is useful when `(setq nnrss-use-local t)'."
  (interactive)
  (insert "#!/bin/sh\n")
  (insert "WGET=wget\n")
  (insert "RSSDIR='" (expand-file-name nnrss-directory) "'\n")
  (dolist (elem nnrss-server-data)
    (let ((url (or (nth 2 elem)
		   (second (assoc (car elem) nnrss-group-alist)))))
    (insert "$WGET -q -O \"$RSSDIR\"/'" 
	    (nnrss-translate-file-chars (concat (car elem) ".xml"))
	    "' '" url "'\n"))))

(defun nnrss-translate-file-chars (name)
  (let ((nnheader-file-name-translation-alist
	 (append nnheader-file-name-translation-alist '((?' . ?_)))))
    (nnheader-translate-file-chars name)))

(defvar nnrss-moreover-url 
  "http://w.moreover.com/categories/category_list_rss.html"
  "The url of moreover.com categories.")

(defun nnrss-snarf-moreover-categories ()
  "Snarf RSS links from moreover.com."
  (interactive)
  (let (category name url changed)
    (with-temp-buffer
      (nnrss-insert nnrss-moreover-url)
      (goto-char (point-min))
      (while (re-search-forward 
	      "<A NAME=\"\\([^\"]+\\)\">\\|<A HREF=\"\\(http://[^\"]*moreover\\.com[^\"]+page\\?c=\\([^\"&]+\\)&o=rss\\)" nil t)
	(if (match-string 1)
	    (setq category (match-string 1))
	  (setq url (match-string 2)
		name (nnweb-decode-entities-string
		      (rfc2231-decode-encoded-string 
		       (match-string 3))))
	  (if category
	      (setq name (concat category "." name)))
	  (unless (assoc name nnrss-server-data)
	    (setq changed t)
	    (push (list name 0 url) nnrss-server-data)))))
    (if changed
	(nnrss-save-server-data ""))))

(defun nnrss-format-string (string)
  (nnweb-replace-in-string (nnrss-string-as-multibyte string) " *\n *" " "))

(defun nnrss-node-text (node)
  (if (and node (listp node))
      (mapconcat 'nnrss-node-text (cddr node) "")
    node))

(provide 'nnrss)

;;; nnrss.el ends here
