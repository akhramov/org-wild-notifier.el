
test:
	cd tests ; emacs --batch --eval '(progn(package-initialize)(add-to-list `package-archives `("melpa" . "https://melpa.org/packages/"))(package-refresh-contents)(package-install-file "../org-wild-notifier.el"))' -l ert  -l org-wild-notifier-tests.el -f ert-run-tests-batch-and-exit
