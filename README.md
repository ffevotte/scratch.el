# scratch

`scratch` is an Emacs extension helping deal with "scratch" buffers, i.e. buffers whose content is mostly ephemeral, but still valuable enough that we don't want to risk killing them inadvertently.


## Installation, setup and usage

### Installation

Just clone the repository somewhere and `require` the package in your init file:

```elisp
(add-to-list 'load-path "/path/to/scratch")
(require 'scratch)
```

### Setup

Simply bind `scratch/switch-to-buffer` (and optionally `scratch/create`) to some keys of your choice. `scratch/switch-to-buffer` is meant to be used as a drop in replacement for `switch-to-buffer` (or its ido counterpart), and should therefore be bound to something like <kbd>C-x</kbd><kbd>b</kbd>.

```elisp
(global-set-key (kbd "C-x b") #'scratch/switch-to-buffer)
(global-set-key (kbd "C-x B") #'scratch/create)
```

### Usage

- `scratch/switch-to-buffer` and `scratch/pop-to-buffer` behave similarly to their non-namespaced counterparts, except when providing the name of a non-existent buffer. In such cases, a new buffer is created and put in `scratch-mode` (see more about what this implies below).<br/>As a special case, if the buffer name argument is left blank, the `*scratch*` buffer is selected by default.

- `scratch/create` always creates a new scratch buffer. When called without argument, the buffer name defaults to `*scratch*<N>`. With a prefix argument, a buffer name is prompted for in the minibuffer.

With the configuration described above, here is how a few operations on scratch buffers might be achieved:

|  | Key sequence |
| ---- | ---- |
| Switch to `*scratch*` | <kbd>C-x</kbd><kbd>b</kbd><kbd>C-j</kbd> |
| Create a new scratch buffer<br/>(automatically named `*scratch*<N>`) | <kbd>C-x</kbd><kbd>B</kbd> |
| Create a new scratch buffer<br/>(named `foo.el`) | <kbd>C-u</kbd><kbd>C-x</kbd><kbd>B</kbd>`foo.el`<kbd>RET</kbd><br><kbd>C-x</kbd><kbd>b</kbd>`foo.el`<kbd>RET</kbd> |

## Discussion

Scratch buffers should provide at least the following features:

1. **Easy creation**: using temporary buffers appears #5 in Steve Yegge's ["10 Specific Ways to Improve Productivity With Emacs"][yegge]. This means that creating a new scratch buffer should be one key sequence away, without having to think about complex things like naming the buffer.

2. **Data loss prevention**: when a scratch buffer is about to be closed, the user should be warned that its contents will be lost unless it is saved to disk.

Stock Emacs' `*scratch*` buffer only partially meets these needs: there initially exists one `*scratch*` buffer (no need to even create it, which is good for point (1) above), but it is not easy to create additional ones. Moreover, the contents of this buffer can easily get lost.

There is actually no easy way to deal with scratch buffers, since neither file buffers nor non-file buffers present them both at the same time. The table below compares the typical behaviour of file buffers (i.e. buffers associated to real files in the filesystem) and non-file buffers (such as stock emacs' `*scratch*` buffer), with the desired behaviour of scratch buffers:

|  | File | Scratch | Non-file |
| :---- | :----: | :----: | :----: |
| **Define file name/path** | buffer creation | *if/when saving* | *if/when saving* |
| **Warn if unsaved** | *data loss risk*<br/>external process | *data loss risk*  | never |
| **Automatic mode** | *yes* | *when named* | no |

- A file path must be provided as soon as a file buffer is created. On the other hand, non-file buffer can live with arbitrary names until they are saved to disk. Only at this point is it necessary to decide where they will be saved. This latter behaviour is required to ease scratch buffers creation.

- Emacs warns the user when file buffers are not synchronized with their associated file on disk. This happens mostly in 2 kinds of situations:
    1. when the buffer is going to be killed, or
    1. when Emacs calls an external command which could potentially be affected by the files contents (e.g. `compile` or `grep`).

  On the other hand, Emacs never tries to prevent data loss on non-file buffers contents. For scratch buffers, an intermediate behaviour makes more sense: data loss should be prevented when the buffers are going to be killed (situation (i) above), but synchronization with the filesystem should not be enforced when calling external commands (situation (ii) above).

- File buffers are automatically put in the correct major mode, based on their file extension (through `auto-mode-alist`). This is a desirable feature for scratch buffers as well (in the cases where one bothers to determine a name at buffer creation).


## References

- [Emacs: problems of the Scratch buffer][lee], by Xah Lee;




[lee]: http://ergoemacs.org/emacs/modernization_scratch_buffer.html "Emacs: problems of the Scratch buffer"

[yegge]: https://sites.google.com/site/steveyegge2/effective-emacs "Effective Emacs"



<!-- Local Variables: -->
<!-- visual-line-mode: t -->
<!-- End: -->
