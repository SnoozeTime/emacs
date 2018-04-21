
(setq elfeed-feeds
      '("http://feeds.feedburner.com/codinghorror"
	"http://importpython.com/blog/feed/"
	"http://pragmaticemacs.com/feed/"
	"https://waitbutwhy.com/feed"))

(defun elfeed-mark-all-as-read()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))
