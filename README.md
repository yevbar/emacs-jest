# emacs-jest

This package provides a way to work with [jest](https://jestjs.io/) in [emacs](https://www.gnu.org/software/emacs/)

## TODO

- [x] Be able to take advantage of defcustom or smth (ie to set maxWorkers etc)
- [x] Be able to request to run with coverage report
- [ ] Parse HTML output with percentages to view org files with coverage results https://emacs.stackexchange.com/a/46372
- [ ] Store historical values (ses files/coverage reports)
- [ ] Make PR to add to Melpa!

## Customization

To customize the variables below, `M-x customize-group RET jest RET`

### jest-coverage-directory

This is the directory that the root `index.html` file is expected (Important: without trailing slash). For `html` coverage reports, this is simply the `coverageDirectory` value in your jest config file. For `lcov` coverage reports, this is the `coverageDirectory` value concatenated with `"/lcov-report"`.

## Working with cells

When presenting code coverage in an org-table, you may want to modify column width but still be able to access cell info. To do so, you can add something like the following to your `init.el` file

```elisp
(defun get-cell-info ()
  (interactive)
  (when (org-table-p)
    (let ((filename (org-table-get nil 1))
	  (column-name (org-table-get 1 nil)))
      (message (string-join (list filename column-name) " - ")))))

(global-set-key (kbd "C-c i") 'get-cell-info)
```

So that you can modify column width using [`<N>` notation](https://orgmode.org/manual/Column-Width-and-Alignment.html) and then press `C-c i` from within the cell you're in and see a message displaying information about the cell

TODO - insert screenshot here
