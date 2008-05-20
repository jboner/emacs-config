#
# $Header: /cvsroot/voicecode/VCode/Admin/pythondoc/HTML4Formatter.py,v 1.1 2001/05/02 19:51:43 dcfox Exp $
#
# Copyright (C) Daniel Larsson
# All Rights Reserved.
#
# See copyright notice in the file 'LICENSE.TXT', which should have accompanied
# this distribution.
#
"""Pythondoc formatter for HTML 4.0

This formatter generates HTML 4.0 files, which basically means it uses
stylesheets and some other newer stuff. The generated pages should pass
the W3C validation service for HTML 4.0. Unfortunately, that doesn't
mean they will look good on all browsers... yet.

You may set some options to influence how the final pages will look
like:

stylesheet -- Select which stylesheet URL to use. There is a default
              stylesheet which is used if this is not set.

header_file -- Should refer to a file containing HTML text suitable for
               insertion right after the <BODY> tag.

footer_file -- Ditto, but inserted right before the </BODY> tag.
"""

__author__ = "Daniel Larsson, Daniel.Larsson@telia.com"

import string, os
import pythondoc.version, pythondoc.doctree, pythondoc.message
import pythondoc.utilities
import format_init
import HTML4strings

S = HTML4strings
formatter_name = "HTML4"

def make_msg(msg):
    return "%s: %s" % (formatter_name, msg)

def init(options):
    global _options
    _options = options
    # Register formatter
    format_init.add_formatter(formatter_name, HTML4Formatter)

    d = os.path.dirname(__file__)

    # Add options
    options.add_option(formatter_name, "frame", options.BOOL,
		       "Generate pages using frames",
		       0)
    options.add_option(formatter_name, "stylesheet", options.URL,
		       "CSS stylesheet URL",
		       'pythondoc.css')
    options.add_option(formatter_name, "header_file", options.FILE,
		       "HTML header code file",
		       os.path.join(d, 'HTML4StdHeader.html'))
    options.add_option(formatter_name, "footer_file", options.FILE,
		       "HTML footer code file",
		       os.path.join(d, 'HTML4ValidateFooter.html'))

    options.add_option(formatter_name, "foobar", options.ENUM,
		       "Test enum",
		       'Foo', ('Foo', 'bar', 'help'))

def element(tag, text, **kw):
    attrs = reduce(lambda c, item: c + ', %s="%s"' % item, kw.items(), '')
    if attrs:
	attrs = ' ' + attrs[2:]
    return '<%s%s>%s</%s>' % (tag, attrs, text, tag)

def open_file(option_name):
    filename = _options.read_value(formatter_name, option_name)[1]
    if filename:
	try:
	    return open(filename).read()
	except IOError:
	    from pythondoc.message import warning
	    warning(make_msg("Cannot open file %s" % filename))
    return ''

def _compare_id(n1, n2):
    return cmp(n1.attribute('id'), n2.attribute('id'))

class DebugFile:
    def __init__(self, filename):
	self.file = open(filename, 'w')
    def write(self, str):
	if not str:
	    raise "ERROR", str
	return self.file.write(str)

class HTML4Formatter:
    css_copied = 0

    def __init__(self, module_tree=None):
	self.in_pre = 0
	self.module_tree = module_tree

    def document(self, doctree):
	"""Generate HTML page(s) for tree.

	Generates one or many HTML pages documenting the given
	doctree object."""

	# Copy default stylesheet if no stylesheet is given by user
	if not self.css_copied:
	    HTML4Formatter.css_copied = 1

	    stylesheet_given = _options.read_value(formatter_name,
						   'stylesheet')[0]
	    if not stylesheet_given:
		dir = _options.read_value('main', 'directory')[1]
		filename = 'pythondoc.css'
		if dir:
		    filename = os.path.join(dir, filename)

		file = open(filename, 'w')
		css_file = os.path.join(os.path.dirname(__file__),
					'HTML4styles.css')
		pythondoc.message.information(make_msg(
		    "Copying CSS file from " + css_file))
		try:
		    file.write(open(css_file).read())
		except IOError, msg:
		    pythondoc.message.error(make_msg(
			"Couldn't find default style sheet file: %s" & css_file))
		file.close()

	self.open_file(doctree)
	exec "self.write_%s(doctree)" % string.lower(doctree.tag())

	author, version = find_specials(doctree)

	footer = open_file('footer_file') % vars()
	self.file.write(S.FILE_END % vars())

    def open_file(self, node):
	"""Opens the .html file and writes the heading."""
	name = pythondoc.utilities.id2name(node.attribute('id'))
	try:
	    package = node.attribute('package')
	    name = package + '.' + name
	except KeyError:
	    pass

	dir = _options.read_value('main', 'directory')[1]
	filename = node.attribute('id')+'.html'
	if dir:
	    import os
	    filename = os.path.join(dir, filename)

	self.file = open(filename, 'w')
	# self.file = DebugFile(node.attribute('id')+'.html')
	
    def write_module(self, node):
	"""Called by the doctree for module nodes."""

	# Write the module header
	modulename = node.attribute('id')
	pythondoc.message.information("Making HTML4 page for %s" % modulename)

	stylesheet = _options.read_value(formatter_name, 'stylesheet')[1]
	header = open_file('header_file')
	self.file.write(S.MODULE_HEADER % vars())

	from pythondoc.utilities import parent_path
	parent = parent_path(modulename)
	if parent:
	    self.file.write(S.DECL_IN % (parent, parent))

	self.file.write('<PRE>')

	# Continue traversing the rest of the tree
	modules = node.children(tag="Module")
	for module in modules:
	    f = HTML4Formatter()
	    f.document(module)

	    oneliner = module.child(tag="Oneliner")
	    if oneliner:
		oneliner = S.ONELINER % self.make_text(oneliner)
	    else:
		oneliner = ''
	    id = module.attribute('id')
	    name = pythondoc.utilities.id2name(id)
	    self.file.write(S.MODMODULE % (id, name, oneliner))

	classes = node.children(tag="Class")
	for clazz in classes:
	    f = HTML4Formatter(node)
	    f.document(clazz)

	    oneliner = clazz.child(tag="Oneliner")
	    if oneliner:
		oneliner = S.ONELINER % self.make_text(oneliner)
	    else:
		oneliner = ''
	    href = clazz.attribute('id')
	    classname = pythondoc.utilities.id2name(href)
	    bases = self.make_classbases(clazz)
	    self.file.write(S.MODCLASS % vars())

	functions = node.children(tag="Function")
	funclist = []
	for fun in functions:
	    oneliner = fun.child(tag="Oneliner")
	    if oneliner:
		oneliner = self.make_text(oneliner)
		oneliner_top = S.ONELINER % oneliner
	    else:
		oneliner = oneliner_top = ''

	    funcname = pythondoc.utilities.id2name(fun.attribute('id'))
	    args = self.get_arguments_doc(fun)[0]

	    if fun.child(tag="Description").children():
		funcref = make_html_id(funcname)
		funclist.append((fun, S.FUNCTION_HEAD % vars()))
		MODFUNC = S.MODFUNC_DOC
	    else:
		MODFUNC = S.MODFUNC

	    self.file.write(MODFUNC % vars())

	self.write_varhead(node)

	aliases = node.children(tag="Alias")
	for alias in aliases:
	    name = pythondoc.utilities.id2name(alias.attribute('id'))
	    type = alias.attribute('type')
	    try:
		aliasfor = alias.attribute('HREF')
	    except:
		aliasfor = '***Unknown***'

	    self.file.write(S.MODALIAS % (name, aliasfor, type))

	self.file.write('</PRE>')


	if node.child(tag='Description').children():
	    self.file.write(element('H2', "Description"))
	    self.write_description(node)

	for func, head in funclist:
	    self.file.write(head + '\n')
	    self.write_description(func)

    def write_class(self, node):
	# Find methods
	methods, methodlist = self.make_methodhead(node)

	# Find inherited methods
	inherits = node.children(tag="Inherited")
	for base in inherits:
	    b_methods, b_methodlist = self.make_methodhead(base)
	    methods = methods + S.INHERITS_FROM % base.attribute('HREF') + b_methods
	    methodlist = methodlist + b_methodlist
	    
	from pythondoc.utilities import parent_path

	classname = pythondoc.utilities.id2name(node.attribute('id'))
	modulename = parent_path(node.attribute('id'))

	pythondoc.message.information("Making HTML4 page for %s" % classname)

	# Find oneliner
	oneliner = node.child(tag="Oneliner")
	if oneliner:
	    oneliner = ' - ' + self.make_text(oneliner)
	else:
	    oneliner = ''

	# Find base classes
	bases = self.make_classbases(node)

	# Make inheritance hierarchy
	hierarchy = self.make_hierarchy(node)
	if hierarchy:
	    hierarchy = element('H2', 'Inheritance hierarchy:',
				CLASS="ClassHierarchy") + \
			element('P', hierarchy, CLASS="ClassHierarchy")

	stylesheet = _options.read_value(formatter_name, 'stylesheet')[1]
	header = open_file('header_file')
	self.file.write(S.CLASS_HEADER % vars())
	self.write_varhead(node)
       	self.file.write(S.CLASS_END)

	if node.child(tag='Description').children():
	    self.file.write(element('H2', "Description"))
	    self.write_description(node)

	for method, head in methodlist:
	    if method.child(tag="Description").children():
		self.file.write(head + '\n')
		argdesc = self.get_arguments_doc(method)[1]
		if argdesc:
		    self.file.write(argdesc)
		self.write_description(method)

    def make_classbases(self, node):
	# Find base classes
	bases = map(lambda n: '<A CLASS="DocLink" HREF="%s.html">%s</A>' \
		    % (n.attribute('HREF'), n.children()[0].text()),
		    node.all(tag='BaseClass'))

	if bases:
	    bases = '(' + reduce(lambda c, b: c + ', ' + b, bases) + ')'
	else:
	    bases = ''

	return bases

    def make_hierarchy(self, node, level=0):
	try:
	    name = node.attribute('id')
	    if level > 0:
		name = '<A HREF="%s.html">%s</A>' % (name, name)
	except KeyError:
	    name = node.attribute('HREF')

	s = '&nbsp;'*2*level + '%s<BR>\n' % name
	bases = node.all(tag='BaseClass')

	# Skip this is the class doesn't inherit anything
	if level == 0 and not bases:
	    return ''

	for base in bases:
	    if self.module_tree:
		basenode = self.module_tree.all(name=base.attribute('HREF'))
		if basenode:
		    base = basenode[0]
	    s = s + self.make_hierarchy(base, level+1)
	return s
	
    def make_methodhead(self, node):
	methodnodes = node.children(tag="Method")
	methodnodes.sort(_compare_id)
	methods = ''
	methodlist = []
	for method in methodnodes:
	    methodname = method.attribute('id')
	    args, args_desc = self.get_arguments_doc(method)

	    oneliner = method.child(tag="Oneliner")
	    if oneliner:
		oneliner = self.make_text(oneliner)
		oneliner_top = S.ONELINER % oneliner
	    else:
		oneliner_top = ''

	    linkstart = linkend = ''
	    if method.child(tag="Description").children():
		methodref = make_html_id(methodname)
		linkstart = '<A CLASS="DocLink" HREF="#%s">' % methodref
		linkend = '</A>'
		methodlist.append((method, S.METHOD_HEAD % vars()))

	    mstr = S.METHOD % vars() + oneliner_top
	    methods = methods + mstr + '\n'

	return methods, methodlist

    def write_varhead(self, node):
	variables = node.children(tag="Variable")
	for var in variables:
	    varname = pythondoc.utilities.id2name(var.attribute('id'))
	    vartype = var.attribute('type')
	    value = self.make_text(var.child(tag='Value'))

	    self.file.write(S.MODVAR % (vartype, varname, value))

    def write_description(self, node):
	description = node.child(tag="Description")
	self.file.write(self.make_text(description))

    def make_text(self, node, level=0):
	s = ''
	for child in node.children():
	    try:
		tag = string.lower(child.tag())
		s = s + eval("self.make_%s(child)" % tag)
		s = s + self.make_text(child, level+1)
		if hasattr(self, 'end_%s' % tag):
		    s = s + eval("self.end_%s(child)" % tag)
	    except AttributeError, msg:
		print "ERROR:", msg
		import sys
		print sys.exc_value, sys.exc_type
		sys.exit(-1)

	return s

    def write_function(self, node):
	from pythondoc.utilities import parent_path

	id = node.attribute('id')
	functionname = pythondoc.utilities.id2name(id)
	funcref = make_html_id(functionname)
	modulename = parent_path(id)
	args, args_desc = self.get_arguments_doc(node)

	# Find oneliner
	oneliner = node.child(tag="Oneliner")
	if oneliner:
	    oneliner = ' - ' + pythondoc.doctree.escape(oneliner.text())
	else:
	    oneliner = ''

	stylesheet = _options.read_value(formatter_name, 'stylesheet')[1]
	header = open_file('header_file')
	self.file.write(S.FUNCTION_HEADER % vars())
       	self.file.write(S.FUNCTION_END)

	if node.child(tag='Description'):
	    self.file.write('\n' + element('H2', "Description") + '\n\n')
	    if args_desc:
		self.file.write(args_desc)
	    self.write_description(node)

    def get_arguments_doc(self, node):
	from pythondoc.doctree import escape
	args = node.all(tag="Argument")
	args_str = ''
	args_desc = ''
	for arg in args:
	    name = pythondoc.utilities.id2name(arg.attribute('id'))
	    type = arg.attributes().get('type', '')
	    if type == 'VARKEYWORDS':
		name = '**'+name
	    elif type == 'VARARGS':
		name = '*'+name

	    default = arg.child(tag="Default")
	    if default:
		name = name + '=' + escape(default.children()[0].text())

	    span = element('SPAN', name, CLASS="Argument")

	    desc = arg.child(tag="Description")
	    if desc:
		id = make_html_id(node.attribute('id') + name)
		args_desc = args_desc + S.ARG_DESC % (id, span, escape(desc.text()))
		span = element('A', span, HREF='#' + id)

	    args_str = args_str + span + ', '

	if args_desc:
	    args_desc = S.ARGS_DESC % args_desc
	else:
	    args_desc = None

	return args_str[:-2], args_desc

    def make_textnode(self, node):
	parent_tag = string.lower(node.parent().tag())
	# try:
	# exec "self.make_%s_textnode(node)" % parent_tag
	# except AttributeError:
	return pythondoc.doctree.escape(node.text())

    def make_emp(self, node):
	return '<EM>'
	
    def end_emp(self, node):
	return '</EM>'
	
    def make_strong(self, node):
	return '<STRONG>'
	
    def end_strong(self, node):
	return '</STRONG>'
	
    def make_code(self, node):
	return '<CODE>'
	
    def end_code(self, node):
	return '</CODE>'

    def make_pycode(self, node):
	self.in_pre = 1
	return '<PRE>'

    def end_pycode(self, node):
	self.in_pre = 0
	return '</PRE>'

    def make_p(self, node):
	if not self.in_pre:
	    return '<P>\n'
	else:
	    return ''

    def end_p(self, node):
	if not self.in_pre:
	    return '</P>\n'
	else:
	    return ''

    def make_ul(self, node):
	return '\n<UL>\n'

    def end_ul(self, node):
	return '\n</UL>\n'

    def make_ol(self, node):
	return '\n<OL>\n'

    def end_ol(self, node):
	return '\n</OL>\n'

    def make_li(self, node):
	return '\n<LI>'

    def make_dl(self, node):
	return '\n<DL>\n'

    def end_dl(self, node):
	return '\n</DL>\n'

    def make_def(self, node):
	return '\n<DD>'

    def make_term(self, node):
	return '\n<DT>\n'

    def end_ol(self, node):
	return '\n</OL>\n'

    def make_heading(self, node):
	return ''

    def make_title(self, node):
	return '\n<H3>'

    def end_title(self, node):
	s = '</H3>\n'
	if node.parent().tag() == 'Example':
	    s = s + '<PRE CLASS="Example">\n'
	self.in_pre = 1
	return s

    def make_example(self, node):
	return ''

    def end_example(self, node):
	self.in_pre = 0
	return '</PRE>\n'

    def make_usage(self, node):
	return element('H3', "Usage", CLASS="Usage") + '\n<DIV CLASS="Usage">'

    def end_usage(self, node):
	return '</DIV>'


    def make_a(self, node):
	try:
	    text, type, url = node.attribute('HREF')
	except:
	    print node
	    raise "Foo"

#DCF
	if type == 'files:':
	    type = 'file:'
#	    pythondoc.message.error(make_msg('warning -- assuming files: means file:'))
	if type not in ('http:', 'ftp:', 'gopher:', 'file:'):
	    try:
		from pythondoc.xref import xref
		xref_resolver = xref.xref_extensions[type]
	    except KeyError:
		pythondoc.message.error(make_msg('No xref handler registered for "%s"' % type))

#		url = xref_resolver.resolve(text, type, url)
# DCF
		url = xref.resolve(text, type, url)
	else:
	    url = type+url
	return '<A HREF="%s">' % url

    def end_a(self, node):
	return '</A>'

    def make_exceptions(self, node):
	return element('H3', 'Exceptions:', CLASS="Exception")

    def index(self, doctree):
	indexer.index(doctree)
	contents.index(doctree)

    def finish(self):
	indexer.finish()
	contents.finish()


def open_outfile(filename, template):
    dir = _options.read_value('main', 'directory')[1]
    if dir:
	import os
	filename = os.path.join(dir, filename)
    file = open(filename, 'w')
    stylesheet = _options.read_value(formatter_name, 'stylesheet')[1]
    header = open_file('header_file')
    file.write(template % vars())
    return file


class HTML4Contents:
    def __init__(self):
	self.__entries = []
	self.file = None

    def index(self, doctree):
	if not self.file:
	    self.file = open_outfile('index.html', S.INDEX_HEADER)
	self._index(doctree)

    def _index(self, doctree):
	tag = string.lower(doctree.tag())
	if tag == 'module':
	    path = doctree.attribute('id')
	    if path not in map(lambda (a, b): a, self.__entries):
		# Find oneliner
		esc = pythondoc.doctree.escape
		oneliner = doctree.child(tag="Oneliner")
		if oneliner:
		    oneliner = ' - ' + esc(oneliner.text())
		else:
		    oneliner = ''

		self.__entries.append((path, oneliner))

	    for child in doctree.children(tag="Module"):
		self._index(child)

    def finish(self):
	self.__entries.sort()
	cur_depth = -1
	for item, oneliner in self.__entries:
	    split = string.split(item, '.')
	    depth = len(split)
	    if depth > cur_depth:
		cur_depth = depth
		self.file.write('<UL CLASS="Contents">\n')
	    elif depth < cur_depth:
		self.file.write((cur_depth-depth)*'</UL>\n')
		cur_depth = depth
	    href = '<A HREF="%s.html">%s</A>' % (item, split[-1])

	    self.file.write('<LI>' + href + oneliner + '\n')

	self.file.write(cur_depth * '</UL>\n')

	author, version = '', ''
	footer = open_file('footer_file') % vars()
	self.file.write(S.FILE_END % vars())


class HTML4Indexer:
    def __init__(self):
	self.__entries = []
	self.file = None
	
    def index(self, doctree):
	if not self.file:
	    self.file = open_outfile('indices.html', S.INDICES_HEADER)
	self._index(doctree)

    def _index(self, doctree):
	tag = string.lower(doctree.tag())
	attr = "index_" + tag
	if hasattr(self, attr):
	    exec "self.%s(doctree)" % attr
	for child in doctree.children():
	    self._index(child)

    def index_module(self, node):
	self.__entries.append((self.__make_entry(node),))
	entry = self.__make_nested_entry(node)
	if entry:
	    self.__entries.append(entry)

    def __make_nested_entry(self, node):
	path = node.attribute('id')
	url = path + '.html'
	entries = []

	from pythondoc.docobjects import parent_package
	package = parent_package(path)
	if package:
	    parent = node.parent()
	    if parent:
		entries = self.__make_nested_entry(parent)
	    entries.append(self.__make_subentry(node))
	else:
	    entries.append(self.__make_entry(node))

	return entries

    def __make_entry(self, node):
	path = node.attribute('id')
	url = path + '.html'

	from pythondoc.docobjects import parent_package
	package = parent_package(path)
	if package:
	    parent = node.parent()
	    if parent:
		typ = parent.tag()
	    else:
		typ = "Module"

	    txt = " (in %s %s)" % (typ, package)
	else:
	    txt = ''

	return (pythondoc.utilities.id2name(path) + ', ' + node.tag() + txt, url)

    def __make_subentry(self, node):
	path = node.attribute('id')
	url = path + '.html'
	return (pythondoc.utilities.id2name(path) + ', ' + node.tag(), url)


    def __add_in_package(self, node, url):
	path = node.attribute('id')
	from pythondoc.docobjects import parent_package
	package = parent_package(path)
	if package:
	    parent = node.parent()
	    if parent:
		typ = parent.tag()
	    else:
		typ = "Module"
	    self.__entries.append(((package + ', ' + typ,
				    pythondoc.utilities.id2name(path) + ', ' \
				    + node.tag()),
				   url))

    index_class = index_module
    index_function = index_module

    def finish(self):
	if not self.file:
	    return

	def compare(e1, e2):
	    # Concatenate all strings
	    s1 = reduce(lambda c, (key, url): c + key, e1, '')
	    s2 = reduce(lambda c, (key, url): c + key, e2, '')
	    return cmp(string.lower(s1), string.lower(s2))
	self.__entries.sort(compare)

	current = '@'
	index_str = ''
	links_str = ''
	last_entry = None
	subkey = None
	for list in self.__entries:
	    key, url = list[0]
	    if string.upper(key[0]) != current:
		current = string.upper(key[0])
		if current == '_':
		    id_current = "x_"
		else:
		    id_current = current
		index_str = index_str + '\n<H2 ID="%s">%s</H2>\n' % (id_current, current)
		links_str = links_str + '<A HREF="#%s">%s</A> ' % (id_current, current)

	    for i in range(len(list)):
		key, url = list[i]
		if not last_entry or len(last_entry) <= i or \
		   key != last_entry[i][0]:
		    index_str = index_str + S.INDEX_ENTRY % (i, url, key)

	    last_entry = list

	self.file.write(links_str + '<BR>\n<HR>\n')
	self.file.write(index_str)
	author, version = '', ''
	footer = open_file('footer_file') % vars()
	self.file.write(S.FILE_END % vars())
	

indexer = HTML4Indexer()
contents = HTML4Contents()

def find_specials(doctree):
    while doctree.parent() and doctree.tag() != "Module":
	doctree = doctree.parent()

    author = doctree.child(name='__author__')
    version = doctree.child(name='__version__')
    if author:
	author = author.child(tag='Value').text()[1:-1]
	# Check for email address
	from pythondoc.docregex import email_regex
	match = email_regex.search(author)
	if match:
	    email = match.group(0)
	    author = author[:match.start()] + \
		     author[match.start() + len(email):]
	    author = string.strip(author)
	    while author[-1] in ',.:;-': author = author[:-1]
	    author = 'Author: <A HREF="mailto:%s">%s</A>' % (email, pythondoc.doctree.escape(author))

    if version:
	version = "Version: %s" % pythondoc.doctree.escape(version.child(tag='Value').text()[1:-1])

    return author or '', version or ''

def make_html_id(id):
    """HTML id's may not start with '_'."""
    id = string.translate(id, string.maketrans('', ''), "='*")
    if id[0] in ('_',):
	return 'X' + id
    else:
	return id

#
# $History: HTML4Formatter.py $
# 
# *****************  Version 11  *****************
# User: Daniel       Date: 98-12-13   Time: 16:20
# Updated in $/Pythondoc/formatters
# Using attribute 'id' instead of 'path'. Some minor fixes done.
# 
# *****************  Version 10  *****************
# User: Daniel       Date: 98-10-06   Time: 22:07
# Updated in $/Pythondoc/formatters
# Added information message for location of CSS file (debugging).
# 
# *****************  Version 9  *****************
# User: Daniel       Date: 98-10-05   Time: 21:34
# Updated in $/Pythondoc/formatters
# Fixed HTML bug in contents file (one </UL> too many).
# Fixed HTML bug in index file, ID attribute value can't start with a "_"
# 
# *****************  Version 7  *****************
# User: Daniel       Date: 98-08-11   Time: 20:48
# Updated in $/Pythondoc/formatters
# Now the formatter builds a string instead of directly writing to the
# file.
# Previously, markup inside e.g. "oneliners" weren't converted to HTML.
# 
# Added a table of contents class.
# 
# Included oneliner under the description header too.
# 
# Added inheritance hierarchy.
# 
# Added some tags to convert (<Usage> and <PyCode>).
# 
# 
# *****************  Version 6  *****************
# User: Daniel       Date: 98-08-06   Time: 21:36
# Updated in $/Pythondoc/formatters
# Added code to make sure the generated HTML is valid.
# 
# *****************  Version 5  *****************
# User: Daniel       Date: 98-08-06   Time: 20:25
# Updated in $/Pythondoc/formatters
# Added argument description to class methods.
# Fixed generation of argument links.
# Added exception documentation.
# 
# *****************  Version 4  *****************
# User: Daniel       Date: 98-08-06   Time: 17:02
# Updated in $/Pythondoc/formatters
# Added sorting of items.
# Improved indexing mechanism.
# Added author and version info.
# Now stores files in the user given directory.
# Show inheritred methods in class documentation.
# 
# *****************  Version 3  *****************
# User: Daniel       Date: 98-08-05   Time: 13:34
# Updated in $/Pythondoc/formatters
# Temporary saved improved index handling. More work needs to be done!
# 
