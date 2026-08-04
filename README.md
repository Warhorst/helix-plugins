# helix-plugins

My personal plugins for the [helix editor](https://helix-editor.com/), using the [steel event system fork](https://github.com/mattwparas/helix/tree/steel-event-system).

These plugins are only designed for personal use and are not installable from forge.

## File Tree (file_tree.scm)
This plugin provides a file tree at the left border of the screen. It is an adapted version of [forest.hx](https://github.com/Ra77a3l3-jar/forest.hx),
and also uses some code from [glyph.hx](https://github.com/Ra77a3l3-jar/glyph.hx). I removed features I don't need, added new features I wanted
and made it look better with my theme (solarized light).

### Features
- Navigation with hjkl
- Add, rename, delete and move files and directories
- IntelliJ inspired controls, like open and close with the same combination, jump to parent directory and close all sub directories when closing a folder
- Simple black and white look (designed for solarized light)
- File reveal
- Showing "important files" at the top of the directory, like a mod.rs file (the list of "important files" is currently hardcoded)

The following features are supported by forest.hx, but not by this plugin:
- Searchbar
- Changing the render side
- Git-Support
- Excluding directories
- Colored Icons and directories
- A dialog-like, alternative version of the tree (called mini in forest.hx)

### Usage
The plugin has 2 commands to open the file tree:
- tree-toggle, which opens / focuses the tree and also closes it when currently focused (which currently only works if a key combination including "1" is used)
- reveal-file, which opens / focuses the tree and reveals the currently open buffer in the file tree

Add to the init.scm:

```scheme
(require "path/to/file_tree.scm")

(keymap (global)
        (normal ("A-1" ":tree-toggle"))
)
 (keymap (global)
         (normal ("A-2" ":reveal-file"))
 )
  
```

Controls when the tree is focused:

- Enter: Open the selected file
- j : Move down with the cursor
- k : Move up with the cursor
- l : Open the currently selected directory
- h : Close the currently selected directory, or jump to the parent directory when at a file. Closing a directory will close all its sub directories
- a : Add a new file (add "/" to the name to create a directory)
- r : Rename the selected file or directory
- d : Delete the selected file or directory
- x : Mark the selected file or directory for movement
- p : Move the marked file to the target directory
