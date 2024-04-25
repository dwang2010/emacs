

# Base


## Useful Bindings

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">M-SPC</th>
<th scope="col" class="org-left">cycle spacing - just one / remove all / restore original</th>
</tr>


<tr>
<th scope="col" class="org-left">M-\</th>
<th scope="col" class="org-left">delete all whitespace around point (ahead / behind)</th>
</tr>


<tr>
<th scope="col" class="org-left">M-^</th>
<th scope="col" class="org-left">join two lines (newline removed) separated by space</th>
</tr>


<tr>
<th scope="col" class="org-left">C-x C-o</th>
<th scope="col" class="org-left">delete blank lines (leaving only one)</th>
</tr>


<tr>
<th scope="col" class="org-left">C-M-\</th>
<th scope="col" class="org-left">indents each line in region as if hitting tab at beginning</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-d</td>
<td class="org-left">same as delete key</td>
</tr>


<tr>
<td class="org-left">M-d</td>
<td class="org-left">delete region (ahead of cursor)</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">M-=</td>
<td class="org-left">display word / character count</td>
</tr>
</tbody>
</table>


## Useful Commands

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">list-faces-display</td>
<td class="org-left">show current font face definition (color / style)</td>
</tr>


<tr>
<td class="org-left">list-colors-display</td>
<td class="org-left">show displayable colors</td>
</tr>
</tbody>
</table>


# Org-Mode


## Helpful Bindings


### General

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">C-ret</th>
<th scope="col" class="org-left">new heading, at end of subtree</th>
</tr>


<tr>
<th scope="col" class="org-left">M-ret</th>
<th scope="col" class="org-left">insert new heading / item / row (next line)</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">M-left/right</td>
<td class="org-left">change current heading level (only)</td>
</tr>


<tr>
<td class="org-left">M-S-left/right</td>
<td class="org-left">change entire subtree heading level</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">M-up/down</td>
<td class="org-left">move subtree order for heading subtree / item + subitems</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-c C-x C-w</td>
<td class="org-left">remove subtree, but save for pasting</td>
</tr>


<tr>
<td class="org-left">C-c C-x C-y</td>
<td class="org-left">paste subtree, with modified heading level</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-c C-w</td>
<td class="org-left">refile entry or region somewhere else</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-c ^</td>
<td class="org-left">sort same level entries</td>
</tr>


<tr>
<td class="org-left">C-c '</td>
<td class="org-left">when in source block, open into major mode, again to close</td>
</tr>


<tr>
<td class="org-left">C-c -</td>
<td class="org-left">toggle line / region as list</td>
</tr>


<tr>
<td class="org-left">C-c *</td>
<td class="org-left">toggle item into subheading of current level</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-u C-c C-c</td>
<td class="org-left">when at head of list, toggles all to checklist</td>
</tr>
</tbody>

<tbody>
<tr>
<td class="org-left">C-c C-o</td>
<td class="org-left">open link</td>
</tr>


<tr>
<td class="org-left">C-c &amp;</td>
<td class="org-left">return to previous location</td>
</tr>
</tbody>
</table>


### Agenda

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">C-c C-d</td>
<td class="org-left">add deadline date (with DEADLINE preface)</td>
</tr>


<tr>
<td class="org-left">C-c C-s</td>
<td class="org-left">add schedule date (with SCHEDULED preface)</td>
</tr>


<tr>
<td class="org-left">C-c .</td>
<td class="org-left">just add date / time</td>
</tr>
</tbody>
</table>

free form syntax for date / time: e.g. 12/20 1pm+1 == 1-2pm

in agenda view: (/) filter (|) remove filters (t) toggle todo state


### Tags

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">C-c C-q</td>
<td class="org-left">add tags</td>
</tr>


<tr>
<td class="org-left">C-c /</td>
<td class="org-left">filter org file based on query (match == tags)</td>
</tr>


<tr>
<td class="org-left">C-c C-c</td>
<td class="org-left">remove sparse tree view + highlighting</td>
</tr>
</tbody>
</table>


### Tables

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">C-c C-c</td>
<td class="org-left">realign table</td>
</tr>


<tr>
<td class="org-left">TAB</td>
<td class="org-left">realign table + move to next field</td>
</tr>


<tr>
<td class="org-left">M-S-right</td>
<td class="org-left">insert column at point</td>
</tr>


<tr>
<td class="org-left">M-S-down</td>
<td class="org-left">insert row at opint</td>
</tr>
</tbody>
</table>


### Visual Emphasis

**bold** (\*)
*italic* (/)
<span class="underline">underline</span> (\_)
`verbatim` (=)
`code` (~)
<del>strike</del> (+)

    leading colon for "small examples" other markings are *off*


# Server Daemon

Configure to run in daemon mode on startup

    emacs --daemon
    # on mac, do this
    emacs --fg-daemon

Alias "emacs" to instead run emacsclient

    # -c : create new frame instead of using current
    # -a : alternative fall back if server broken / not running
    emacsclient -c -a "emacs"


# Misc

regexp standards
<https://en.wikipedia.org/wiki/Regular_expression#POSIX_basic_and_extended>

