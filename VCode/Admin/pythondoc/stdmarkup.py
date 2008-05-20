#
# $Header: /cvsroot/voicecode/VCode/Admin/pythondoc/stdmarkup.py,v 1.1 2001/05/02 19:51:43 dcfox Exp $
#
# Copyright (C) Daniel Larsson
# All Rights Reserved.
#
# See copyright notice in the file 'LICENSE.TXT', which should have accompanied
# this distribution.
#
"""Standard markup rules for docstrings.

This module contains classes for interpreting the docstrings the
*standard* way, i.e. using Jim Fulton's structured text, with
some additional markup rules.
"""

import types, re

import docstring, doctree, docobjects, docregex
import StructuredText, message

# Constants to control repr.Repr output
REPR_MAXSTRING = 60
REPR_MAXLONG = 80

# Internal constants
UN_LIST, OR_LIST, DEF_LIST = 'UL', 'OL', 'DL'

class Markup:
    dumb_value_re = re.compile("<(?P<value>/S+) object at [0-9a-f]+>")

#DCF
    def __init__(self):
	self.hyperdefs = {}

    def trace_msg(self, msg, lvl=1):
	message.information('*** Markup: %s' % msg, lvl)

    def create_doctree(self, docobj):
	"""Create the doctree.

	Parses the docobject's docstring and builds a doctree, which may then
	be translated to various formats.
	"""
        self.__doctree = eval("self.create_head_%s(docobj)" % docobj.tag())
        eval("self.create_tree_%s(docobj)" % docobj.tag())
        return self.__doctree

    def create_head_Object(self, docobj):
	"""Default implementation of doctree head creation."""
	attrs = docobj.attributes()
	if not attrs.has_key('id'):
	    attrs['id'] = docobj.path()
	head = apply(doctree.DocNode,
                     ('', docobj.tag(), docobj.sort_order()),
		     attrs)
	# Don't create 'Name' nodes anymore!
	# The name is stored in the 'id' attribute instead
	# head.add_child(doctree.Name(docobj.name()))
	return head

    create_head_Module = create_head_Object
    create_head_Class = create_head_Object
    create_head_ModuleRef = create_head_Object
    create_head_BuiltinFunction = create_head_Object
    create_head_Alias = create_head_Object

    def create_head_ObjectWithArgs(self, docobj):
	"""Create the top doctree for objects with arguments."""
        head = self.create_head_Object(docobj)
	args = _doc_arglist(docobj.funcobject())
	if args:
	    head.add_child(args)
	return head

    create_head_Function = create_head_ObjectWithArgs
    create_head_Method = create_head_ObjectWithArgs

    def create_head_Variable(self, docobj):
        from doctree import Value
	import repr
	myrepr = repr.Repr()
	myrepr.maxstring = REPR_MAXSTRING
	myrepr.maxlong = REPR_MAXLONG
	head = self.create_head_Object(docobj)
	if type(docobj.object()) == types.InstanceType:
	    clsobject = docobjects.create_docobject(docobj.object().__class__)
	    value = clsobject.path() + " instance"
	    head.add_attribute('HREF', clsobject.path())
	else:
	    value = myrepr.repr(docobj.object())

	    m = self.dumb_value_re.match(value)
	    if m:
		value = m.group('value')
	    
	head.add_child(Value(value))
	return head

    def create_tree_Object(self, docobj):
        """Default implementation for generating doctrees."""
	self.trace_msg("Creating tree for %s %s" % (docobj.tag(), docobj.name()), 2)
	oneliner, doc = docstring.split_doc(docobj.docstring())
	try:
            oneliner = eval("self.fix_oneliner_%s(oneliner)" % docobj.tag())
	except AttributeError:
            pass

	doc, hyperdef_doc = docstring.split_hyper(doc)

	st = StructuredText.StructuredText(doc)
	if oneliner:
	    node = doctree.Oneliner('')
	    self._create_markup_node(oneliner, node)
	    self.__doctree.add_child(node)

	marker = doctree.DocNode('', 'Marker')
	self.__doctree.add_child(marker)
	for child in docobj.subobjects():
	    dtree = child.document(Markup)
	    self.__doctree.add_child_sorted(dtree,
                                            after=marker)
	self.__doctree.remove_child(marker)

	desc = doctree.Description()
	self.__doctree.add_child(desc)
	self.__parse_hyperdef(hyperdef_doc, docobj)
	self.__parse(st.structure, desc, docobj)
#	if len(desc.children()) > 0:
#	    self.__doctree.add_child(desc)

    create_tree_Module = create_tree_Object
    create_tree_ModuleRef = create_tree_Object
    create_tree_BuiltinFunction = create_tree_Object
    create_tree_Alias = create_tree_Object
    create_tree_Function = create_tree_Object
    create_tree_Method = create_tree_Object
    create_tree_Variable = create_tree_Object

    def create_tree_Class(self, docobj):
	self.create_tree_Object(docobj)
	ix = len(self.__doctree.children())
	bases = doctree.BaseClasses()
        for base in docobj.bases():
	    try:
		bases.add_child(doctree.BaseClass(base.name(),
						  HREF=base.path()), ix)
		ix = ix + 1
	    except AttributeError:
		print type(base.object()), base.object()
		import sys
		raise sys.exc_type, sys.exc_value, sys.exc_traceback

	if bases.children():
	    self.__doctree.add_child(bases)

	for clazz, methods in docobj.inherited_attrs().items():
	    node = doctree.Inherited(HREF=clazz.path())
	    self.__doctree.add_child(node)
	    for method in methods:
		node.add_child(method.document(Markup))

    def create_tree_ModuleRef(self, docobj):
        pass # No need to create more than head

    def fix_oneliner_BuiltinFunction(self, oneliner):
	m = docregex.builtin_head_regex.match(oneliner)
	node = doctree.Arguments()
        
	from string import split, strip, find
        
	args = m.group('args')
	args = map(strip, split(args, ','))
	for arg in args:
	    # optional argument: "[arg]"
	    if arg[0] == '[' and arg[-1] == ']':
		arg = arg[1:-1]
		argnode = doctree.Argument(id=arg, optional=1)
	    # default value: "arg=foo"
	    elif find(arg, '=') != -1:
		[arg, default] = split(arg, '=')
		argnode = doctree.Argument(id=arg)
		argnode.add_child(doctree.Default(default))
	    else:
		argnode = doctree.Argument(id=arg)

	    # argnode.add_child(doctree.Name(arg))
	    node.add_child(argnode)
	self.__doctree.add_child(node)
	return m.group('docstring')

    def __parse_hyperdef(self, hyperdef_doc, docobj):
	self.hyperdefs = {}
	from regex_util import RegexSearchIter
	for rexp in RegexSearchIter(hyperdef_doc, docregex.hyperdef_regex):
	    text, linkmode, link = rexp.group('text',
					      'linkmode',
					      'hyperlink')
	    
	    self.__visit_hyperdef(docobj, text, linkmode, link)



    def __parse(self, st, parent_node, docobj):
	"""Parses the docstring and build a doctree."""
	from regex_util import RegexIter
	import string

	listtype = None

	for text, sub in st:
	    found = 0
	    # Check for bullet lists
	    match = docregex.bullet_regex.match(text)
	    if match:
		if listtype and listtype != UN_LIST:
		    parent_node = parent_node.parent()
		if listtype != UN_LIST:
		    node = doctree.UL()
		    parent_node.add_child(node)
		    parent_node = node
		listtype = UN_LIST

		# Allow bullet list without intervening '\n\n'
		bullet = match.group("bullet")
		l = string.splitfields(text, bullet)[1:]
		for item in l:
		    self.__add_markup_child(item, parent_node, doctree.LI())

		self.__parse(sub, parent_node, docobj)
		continue

	    # Check for ordered lists
	    match = docregex.ol_regex.match(text)
	    if match:
		if listtype and listtype != OR_LIST:
		    parent_node = parent_node.parent()
		if listtype != OR_LIST:
		    node = doctree.OL()
		    parent_node.add_child(node)
		    parent_node = node
		listtype = OR_LIST

		text = match.group('text')
		self.__add_markup_child(text, parent_node, doctree.LI())
		self.__parse(sub, parent_node, docobj)
		continue

	    # Check for ordered lists (alternative syntax)
	    match = docregex.olp_regex.match(text)
	    if match:
		if listtype and listtype != OR_LIST:
		    parent_node = parent_node.parent()
		if listtype != OR_LIST:
		    node = doctree.OL()
		    parent_node.add_child(node)
		    parent_node = node
		listtype = OR_LIST

		text = match.group('text')
		self.__add_markup_child(text, parent_node, doctree.LI())
		self.__parse(sub, node, docobj)
		continue

	    # Check for definition lists
	    match = docregex.dl_regex.match(text)
	    if match:
		if listtype and listtype != DEF_LIST:
		    parent_node = parent_node.parent()
		if listtype != DEF_LIST:
		    node = doctree.DL()
		    parent_node.add_child(node)
		    parent_node = node
		listtype = DEF_LIST
		#

		t,d = match.group("term", "definition")
		match = docregex.dl_regex.search(d)
		while match:
		    self.__add_term(t, parent_node)
		    self.__add_def(d[:match.start()-1], parent_node)
		    t,d = match.group("term", "definition")
		    match = docregex.dl_regex.search(d)

		self.__add_term(t, parent_node)
		self.__add_def(d, parent_node)
		self.__parse(sub, parent_node, docobj)
		continue

	    if docregex.pycode_regex.search(text):
		if listtype:
		    parent_node = parent_node.parent()
		listtype = None
		node = doctree.PyCode(text)
		parent_node.add_child(node)
		self.__parse(sub, node, docobj)
		continue

	    # Check for 'example' headings
	    if sub and docregex.example_regex.search(text):
		if listtype:
		    parent_node = parent_node.parent()
		listtype = None
		node = doctree.Example()
		self.__add_markup_child(text, node, doctree.Title())
		parent_node.add_child(node)
		self.__parse(sub, node, docobj)
		continue

	    # Check for 'Arguments:' section
	    if docregex.arguments_regex.match(text):
		if listtype:
		    parent_node = parent_node.parent()
		listtype = None
		self.__argument_doc(text, sub, docobj)
		continue

	    # Check for 'Exceptions:' section
	    if docregex.exception_regex.match(text):
		if listtype:
		    parent_node = parent_node.parent()
		listtype = None
		self.__exception_doc(text, sub, parent_node, docobj)
		continue

	    # Check for 'Usage:' section
	    if docregex.usage_regex.match(text):
		if listtype:
		    parent_node = parent_node.parent()
		listtype = None
		node = self.__usage_doc(text, sub, parent_node)
		self.__parse(sub, node, docobj)
		continue

	    # Check for 
	    if text:
		if listtype:
		    parent_node = parent_node.parent()
		listtype = None

		if sub and sub[0] and sub[0][0]:
		    pnode = doctree.Heading()
		    node = doctree.Title()
		    pnode.add_child(node)
		else:
		    node = doctree.P()
		    pnode = None

		self._create_markup_node(text, node)
		if pnode:
		    node = pnode

		parent_node.add_child(node)
		self.__parse(sub, node, docobj)

	if listtype:
	    parent_node = parent_node.parent()
	listtype = None

    def __add_markup_child(self, text, parent, n):
	self._create_markup_node(text, n)
	parent.add_child(n)

    def __add_term(self, term, parent):
	self.__add_markup_child(term, parent, doctree.Term())

    def __add_def(self, term, parent):
	self.__add_markup_child(term, parent, doctree.Def())
	    
    def __argument_doc(self, text, structure, docobj):
	"""Creates argument description nodes.

	Extracts argument documentation from the docstring and
	stores them with the arguments in the doctree.
	"""
	from regex_util import RegexIter
	for text, sub in structure:
	    for match in RegexIter(text, docregex.dl_regex):
		argname = match.group('term')
		argdesc = match.group('definition')
		nodes = self.__doctree.all(argname)
		try:
		    node = nodes[0]
	            node.add_child(doctree.Description(argdesc))
		except:
		    self.trace_msg("Failed to find argument '%s'" % argname, 0)
		    self.trace_msg("Argument description skipped", 0)

    def __exception_doc(self, text, sub, parent_node, docobj):
	node = doctree.Exceptions()
	parent_node.add_child(node)
	self.__parse(sub, node, docobj)

    def __usage_doc(self, text, sub, parent_node):
	node = doctree.Usage()
	parent_node.add_child(node)
	return node

    def __check_list(parent_node, stop_list, in_list):
	if stop_list:
	    self.__end_list(parent_node, listtype, in_list)
	    self.__list = []

    def __visit_hyperdef(self, docobj, text, linkmode, path):
	"Called when a hyperlink definition is found."
	# Create internal dict of hyperlink definitions
	self.trace_msg("Adding hyperlink definition: (%s, %s, %s)" \
		       % (text, linkmode, path), 2)
	if linkmode == '': # Internal link!
	    from xref import xref
	    path = xref.xref_extensions[''].resolve_to_path(docobj.object(),
							    path,
							    docobj.globals(),
							    docobj.locals())
	self.hyperdefs[text] = (linkmode, path)

    def _create_markup_node(self, text, parent):
	from regex_util import MultiRegexSearchIter
	iter = MultiRegexSearchIter(text, _text_markup_list)
	pos = 0

	for match in iter:
	    markup_text = match.group('text')

	    # If this is a hyperlink, make sure there exists a definition
	    if match.re == docregex.hypertext_regex:
		self.trace_msg("Found hypertext link: %s" % markup_text, 2)
		if not self.hyperdefs.has_key(markup_text):
		    # Move back iterator to compensate for the "<end_char>"
		    iter.pos = iter.pos - len(match.group("end_char"))
		    continue

	    if match.start() > 0:
		t = text[pos:match.start()] + match.group("start_char")
		parent.add_child(t)
	    node = _text_markups[match.re](markup_text)

	    # Set 'HREF' attribute if this is an anchor!
	    if match.re == docregex.hypertext_regex:
		node.add_attribute('HREF',
				   (markup_text,) + self.hyperdefs[markup_text])

	    parent.add_child(node)
	    # Move back iterator to compensate for the "<end_char>"
	    iter.pos = iter.pos - len(match.group("end_char"))
	    pos = iter.pos

	if iter.pos == 0:
	    parent.add_child(text)
	else:
	    t = text[pos:] #match.group("end_char") + text[match.end():]
	    if t:
		parent.add_child(t)


def _doc_arglist(func):
    """Builds doctree for a function's arguments."""
    from codeutil import *

    stdargs, defaults, varargs = getarguments(func)

    if not (stdargs or defaults or varargs): return None

    from doctree import Arguments, Argument, Default
    node = Arguments()
    for name in stdargs:
	argnode = Argument(id=name)
	# argnode.add_child(Name(name))
	node.add_child(argnode)

    for name, default in defaults:
	argnode = Argument(id=name)
	# argnode.add_child(Name(name))
	argnode.add_child(Default(repr(default)))
	node.add_child(argnode)

    for argtype, name in varargs:
	if argtype == CO_VARARGS:
	    argtype = "VARARGS"
	else:
	    argtype = "VARKEYWORDS"
	argnode = Argument(id=name, type=argtype)
	# argnode.add_child(Name(name))
	node.add_child(argnode)

    return node


# Can't use the dictionary keys, since 'strong_regex' *must*
# come before 'emph_regex'!!!
_text_markup_list = [
    docregex.strong_regex,
    docregex.emph_regex,
    docregex.code_regex,
    docregex.hypertext_regex]

_text_markups = {
    docregex.strong_regex: doctree.Strong,
    docregex.emph_regex: doctree.Emp,
    docregex.code_regex: doctree.Code,
    docregex.hypertext_regex: doctree.A}


#
# $History: stdmarkup.py $
# 
# *****************  Version 6  *****************
# User: Daniel       Date: 98-12-13   Time: 16:23
# Updated in $/Pythondoc
# The 'path' attribute is now removed and replaced with the 'id'
# attribute.
# 
# *****************  Version 5  *****************
# User: Daniel       Date: 98-10-04   Time: 19:45
# Updated in $/Pythondoc
# Removed creation of <NAME> nodes. The name is already stored in the
# 'id' field.
# 
# *****************  Version 4  *****************
# User: Daniel       Date: 98-08-11   Time: 20:59
# Updated in $/Pythondoc
# - Fixed bug in '__parse_hyperdef'. Only the first hyperlink definition
# was found
# - Added check for 'PyCode' text.
# - Fixed "Usage" section bug. Forgot to implement function...
# 
# 
# 
# *****************  Version 3  *****************
# User: Daniel       Date: 98-08-06   Time: 17:30
# Updated in $/Pythondoc
# Added '_Verbose' class for debug messages
# Fixed handling of cross reference definitions.
# Fixed a bug in handling inherited classes.
# Markup didn't work in certain circumstances, such as inside terms in
# definition lists. Fixed.
# Also fixed a copy-paste bug in the code that generates the doctree for
# arguments.
# 
# *****************  Version 2  *****************
# User: Daniel       Date: 98-07-31   Time: 2:46
# Updated in $/Pythondoc
# Added handling of hypertext links. Fixed numerous problems. Previous
# version was not tested much...
# 
