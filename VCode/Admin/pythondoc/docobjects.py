#
# $Header: /cvsroot/voicecode/VCode/Admin/pythondoc/docobjects.py,v 1.1 2001/05/02 19:51:43 dcfox Exp $
#
# Copyright (C) Daniel Larsson
# All Rights Reserved.
#
# See copyright notice in the file 'LICENSE.TXT', which should have accompanied
# this distribution.
#
"""Documentation objects.

This module contains classes reflecting upon objects in python
modules, and is used to extract information for documentation
purposes. To create a *docobject* for a python object, call the
'create_docobject' function, which returns an instance of the
appropriate class. See the documentation on the 'Object' class,
and its subclasses for the kind of information you can obtain.
"""

# Standard modules
import re, imp, types, string, sys

# pythondoc modules
import StructuredText
import docstring, doctree, docregex, message

__author__ = "Daniel Larsson, Daniel.Larsson@telia.com"
__version__ = "$Revision: 1.1 $"[11:-2]

class Object:
    """Base class for document objects"""

    # Public methods
    def __init__(self, object, name=None):
	self.__object = object
	if name:
	    self.__name = name

	# Force generation of children
	self.subobjects()

    def trace_msg(self, msg, lvl=1):
	message.information('*** %s %s: %s' % (self.tag(), self.path(), msg),
			    lvl)

    def docstring(self):
	"""Returns the python object's docstring."""
	try:
	    return self.__object.__doc__
	except AttributeError:
	    return ''

    def document(self, markup_class):
	"""Generate document tree and return it"""
	try:
	    return self.__doctree
	except:
	    #self.trace_msg("Documenting %s %s" % (self.tag(), self.path()))
            self.__doctree = markup_class().create_doctree(self)
	    return self.__doctree

    def name(self):
	"""Returns the name of the python object."""
	try:
	    return self.__name
	except AttributeError:
	    return self.object().__name__

    def path(self):
	name = self.name()
	if self.parent():
	    name = self.parent().path() + '.' + name
	return name

    def internalXref(self):
	return "%s.html#%s" % (self.parent().path(), self.name())

    def object(self):
	"""Returns the Python object I represent"""
	return self.__object

    def parent(self):
	"""Returns documentation object that contains this object."""
	try:
	    return self.__parent
	except AttributeError:
	    self.__parent = self.get_parent()
	    return self.__parent

    def subobjects(self):
	"""Returns list of subobjects, or None"""
	try:
	    return self.__subobjects
	except AttributeError:
	    self.__subobjects = self.get_subobjects()
	    return self.__subobjects

    # Subclass/internal methods
    def set_parent(self, parent):
	self.__parent = parent

    def tag(self):
	return self.__class__.__name__

    def attributes(self):
	try:
	    return {'id': self.path()}
	except:
	    return {}

    def get_parent(self):
	"""Calculates the parent object.

	Don't call this method, use 'parent()'."""
	return None

    def get_subobjects(self):
	"""Returns the subobjects (children) of this object.

	This method returns an empty list. Subclasses may
	override it (notably the Module and Class classes)."""
	return []

    def sort_order(self):
	"""List for ordering children in the doctree.

	Default is alphabetical sorting.
	"""
	try:
	    return self.SORT_ORDER
	except:
	    return None

    def globals(self):
	if self.parent():
	    # Not quite accurate!
	    return self.parent().locals()
	else:
	    return {}

    def locals(self):
	return {}

class Composite(Object):
    """Document object for objects with children."""

    def __init__(self, object, name=None):
	# Insert myself in map, if I'm the first created object
	if not _object_map.has_key(object):
	    try:
		_object_map[object] = self
	    except:
		pass
	Object.__init__(self, object, name)

    def internalXref(self):
	return "%s.html" % self.path()

    def skip_child(self, name, object):
	"""Determines whether to skip a child or not."""
	return name in SKIP_NAMES

    def make_childobject(self, name, object):
	"""Creates documentation object for the given Python object."""
	return create_docobject(object, name)

    def get_subobjects(self):
	"""Calculates the children objects for this object."""
	items = self.get_allobjects()
	subobjs = []
	for key, object in items:
	    if self.skip_child(key, object):
		self.trace_msg("Skipping %s (skip_child)" % key, 2)
		continue
	    subobj = self.make_childobject(key, object)
	    if subobj.parent() in [None, self]:
		subobjs.append(subobj)
		# Integers, lists, and such have no way
		# of figuring out their enclosing object (parent)
		# so we force it. But don't do it on toplevel
		# modules!!
		if not issubclass(subobj.__class__, Module) \
		   and not issubclass(subobj.__class__, ModuleRef):
		    subobj.set_parent(self)
	    else:
		try:
		    parent_name = subobj.parent().name()
		except AttributeError:
		    parent_name = subobj.parent().object()
		self.trace_msg("Skipping %s, childs parent (%s) is not me" %
			       (key, parent_name), 2)
		self.trace_msg("%s %s" % (self.parent(), subobj.parent()), 2)

	return subobjs

    def locals(self):
	return self.object().__dict__


class Module(Composite):
    """Document object for python modules."""

    SORT_ORDER = ['Class', 'Function', 'Variable']

    def __init__(self, object, name=None):
	if not name:
	    from string import split
	    self.__parent_package = parent_package(object.__name__)
	    if self.__parent_package:
		name = object.__name__[len(self.__parent_package)+1:]
	Composite.__init__(self, object, name)

    def get_parent(self):
	module_name = parent_package(self.object().__name__)
	if not module_name:
	    return None
	#exec "import %s" % module_name
	module = do_import(module_name)
	return create_docobject(module)

    def get_allobjects(self):
	"""Returns the python object's all children.

	This is done differently in modules and classes, since
	class member functions are returned as functions when
	going through the class's '__dict__' attribute, and not
	as methods."""
	from os import path, listdir
	children = []
	file = path.split(self.object().__file__)[1]
	if path.splitext(file)[0] == '__init__':
	    for d in self.object().__path__:
		files = listdir(d)
		dirs = filter(path.isdir,
			      map(lambda f, d=d, p=path: p.join(d, f), files))
		pyfiles = filter(lambda f, p=path: p.splitext(f)[1] == '.py',
				 files)
		for pyfile in pyfiles:
		    if pyfile == '__init__.py':
			continue
		    modulename = self.path() + '.' + path.splitext(pyfile)[0]
		    module=None
		    if not re.match(r'^wxPython', modulename):
		      module = do_import(modulename)
		    if not module:
			self.trace_msg("Skipping %s (module not imported)" % module, 2)
			continue
		    if module.__name__ != modulename:
			module.__name__ = modulename
		    children.append((modulename, module))

		for p in dirs:
		    modulename = self.path() + '.' + path.basename(p)
		    module = do_import(modulename)
		    children.append((modulename, module))
	    
	dict = self.object().__dict__
	return children + dict.items()

    def make_childobject(self, name, object):
	"""Creates documentation object for the given Python object.

	Module objects found in this module are mapped to the ModuleRef
	class.
	"""
	# If this is a module, and this module is not a submodule,
	# make it just a reference!
	if type(object) == types.ModuleType and \
	   self.path() != name[:len(self.path())]:
	    oname = string.split(object.__name__, '.')[-1]
	    if name != oname:
		return Alias(object, name)
	    else:
		return ModuleRef(object, name)
	else:
	    return Composite.make_childobject(self, name, object)

    def skip_child(self, name, object):
	"""We skip builtin functions, unless this is a builtin module."""
	return Composite.skip_child(self, name, object) or \
	       not imp.is_builtin(self.name()) and \
	       type(object) == types.BuiltinFunctionType

    def globals(self):
	return self.locals()


class Class(Composite):
    """Document object for Python classes."""

    SORT_ORDER = ['BaseClass', 'Method', 'Function', 'Variable']

    def get_parent(self):
	module_name = self.object().__module__
	#exec "import %s" % module_name
	module = do_import(module_name)
	return create_docobject(module)

    def get_allobjects(self):
	m = {}
	self.__bases = []
	for base in self.object().__bases__:
	    all_methods(base, m)
	    self.__bases.append(create_docobject(base))
	n = {}
	for clazz, methods in m.items():
	    docclass = create_docobject(clazz)
	    docmethods = map(create_docobject, methods)
	    n[docclass] = docmethods
	    
	self.__inherited = n
	keys = self.object().__dict__.keys()
	return map(lambda e, c=self.object(): (e, getattr(c, e)), keys)

    def bases(self):
        return self.__bases

    def inherited_attrs(self):
        return self.__inherited


class ModuleRef(Object):
    """Document object for modules imported from a module."""

    def path(self):
	return self.object().__name__


class ObjectWithArgs:
    """Base class for documentation objects with arguments."""

    # By default the function object is the same as the python object
    # (not true for method objects)
    def funcobject(self):
        return self.object()

    def globals(self):
	return self.funcobject().func_globals


class Function(ObjectWithArgs, Object):
    """Document object for Python functions."""

    def get_parent(self):
	module_name = self.object().func_globals["__name__"]
	module = do_import(module_name)
	return create_docobject(module)


class BuiltinFunction(Object):
    """Document object for Python builtin functions."""
    pass


class Method(ObjectWithArgs, Object):
    """Document object for Python methods."""

    def funcobject(self):
	return self.object().im_func

    def get_parent(self):
	return create_docobject(self.object().im_class)

    def locals(self):
	return self.get_parent().locals()

class Variable(Object):
    """Document object for Python variables."""


    # To determine the type of a variable, we do `type(var)`
    # to convert the type to a string. Then we extract the
    # type string with this regular expression
    extract_type_re = re.compile("<type '(?P<type>.*)'>")

    def __repr__(self):
	return "%s (%s)" % (self.name(), self.object())

    def attributes(self):
	attrs = Object.attributes(self)
	typestr = str(type(self.object()))
	m = self.extract_type_re.match(typestr)
	attrs['type'] = m.group('type')
	return attrs

    def docstring(self):
	return ''

class Alias(Variable):
    """Document object for aliases.

    An alias is a variable referring to an object that itself
    has a name (functions, classes, modules, etc).

    Example:

      # A class
      class Parrot:
        ...
      NorwegianBlue = Parrot # Alias
    """

    def __init__(self, object, name):
	Object.__init__(self, object, name)
	self.__aliasFor = object

    def docalias(self):
	"Return the document object for the aliased object"
	try:
	    return create_docobject(self.__aliasFor)
	except:
	    return None

    def attributes(self):
	attrs = Variable.attributes(self)
	alias = self.docalias()
	if alias:
	    name = alias.path()
	    attrs['HREF'] = name

	return attrs

    def docstring(self):
	try:
	    return 'Alias name for %s.' % self.__aliasFor.__name__
	except:
	    return ''
	    

# Support routines

def split_package(modulename):
    """Splits a module name into package, and module"""
    from string import rindex
    try:
	ix = rindex(modulename, '.')
	return modulename[:ix], modulename[ix+1:]
    except ValueError:
	return None, modulename

def parent_package(modulename):
    """Returns the parent package name of given module.
    'None' is returned if module has no parent"""
    return split_package(modulename)[0]

def _private_name(name, pyobject):
    try:
	return '_' + pyobject.im_class.__name__ + pyobject.__name__ == name
    except AttributeError:
	return 0

def _not_equal_name(name, object):
    if name:
      if not hasattr(object, '__name__'):
	return 0
      if name != object.__name__ and \
       (type(object) != types.ModuleType or \
	name != object.__name__[-len(name):]):
	return 1
    return 0

def create_docobject(pyobject, name=None):
    """Looks for object in '_object_map' and returns it.
    If the object isn't found, a new is created"""
    try:
	# Is this just an alias?
	if _not_equal_name(name, pyobject) and \
	   not _private_name(name, pyobject):
	    return Alias(pyobject, name)
    except AttributeError:
	pass

    try:
	if _object_map.has_key(pyobject):
	    object = _object_map[pyobject]
	    return object
	else:
	    if type(pyobject) == types.ModuleType:
		# Hmm, it seems Python doesn't always
		# make a module's __name__ attribute reflect
		# the fact that it's inside a package...
		for pobject, object in _object_map.items():
		    if type(pobject) == types.ModuleType and \
		       pobject.__file__ == pyobject.__file__:
			return object

	    object = _class_map[type(pyobject)](pyobject) #, name)
	    _object_map[pyobject] = object
	    return object
    except KeyError:
	pass
    except TypeError:
	pass

    # Take care of extension types
    if _isClass(pyobject):
	object = Class(pyobject)
	_object_map[pyobject] = object
	return object

    return Variable(pyobject, name)

def do_import(modulename):
    if type(modulename) != types.StringType:
	return

    try:
	return sys.modules[modulename]
    except KeyError:
	pass

    message.information("Importing module %s" % modulename, 2)
    try:
	__import__(modulename)
	return do_import(modulename)
    except:
	message.error("Couldn't import %s (%s: %s)" % (modulename, sys.exc_type, sys.exc_value))
	
    return None

def _isClass(cls):
    """Determines if 'cls' is a class.

    A class is either a standard Python class object, or an
    extension type mimicking the class protocol."""
    try:
	cls.__bases__
	return 1
    except:
	return 0

def all_methods(clazz, methoddict=None):
    """Return all methods of a class.

    Returns a dictionary *class:[methods]*, containing all
    inherited methods. Overridden methods are removed from
    base classes.
    """

    if methoddict is None:
	methoddict = {}
    for base in clazz.__bases__:
	all_methods(base, methoddict)

    keys = clazz.__dict__.keys()
    items = map(lambda e, c=clazz: getattr(c, e), keys)
    methods = filter(lambda k: type(k) == types.MethodType, items)

    # Remove overridden methods in all base classes
    for mlist in methoddict.values():
	for m in methods:
	    if m in mlist:
		mlist.remove(m)

    # Add myself to dictionary
    methoddict[clazz] = methods
    return methoddict


# Keeps mapping between python object and its
# documentation object
_object_map = {}

# Maps python type to documentation class to use
_class_map = {
    types.FunctionType: Function,
    types.BuiltinFunctionType: BuiltinFunction,
    types.ModuleType: Module,
    types.ClassType: Class,
    types.UnboundMethodType: Method
    }

# Names to skip when picking up children
SKIP_NAMES = ('__doc__', '__builtins__', '__name__',
'__module__','wxPython.lib')

# Testing

def test():
    import pickle, doctree, StructuredText, xml.parsers.xmlproc.xmlproc
    m = Module(xml.parsers.xmlproc.xmlproc)
    import cStringIO
    n = Module(StructuredText)

    print
    tree = m.document()

    import formatters.HTML4Formatter
    f = formatters.HTML4Formatter.HTML4Formatter()
    tree.add_subscriber(f)
    tree.traverse()
    print tree
    print "====================================="
    #print tree.all("DocNode")[0]


def test2():
    import pickle
    m = Method(pickle.Pickler.__init__)
    tree = m.document()
    print tree.xml_source()

if __name__ == "__main__":
    import sys
    sys.stdout = open("output.txt", 'w')
    sys.stderr = sys.stdout
    test()


#
# $History: docobjects.py $
# 
# *****************  Version 6  *****************
# User: Daniel       Date: 98-12-13   Time: 16:31
# Updated in $/Pythondoc
# Replaced attribute 'path' with 'id'.
# New email address.
# 
# *****************  Version 5  *****************
# User: Daniel       Date: 98-10-06   Time: 22:05
# Updated in $/Pythondoc
# Turned the import fail warning into an error.
# 
# *****************  Version 4  *****************
# User: Daniel       Date: 98-08-11   Time: 21:08
# Updated in $/Pythondoc
# - Trying to overcome "bug" when importing modules inside packages.
# Sometimes the __name__ attribute doesn't reflect the fact that the
# module is inside a package.
# 
# - Added 'path' to the ModuleRef class to include the path :-)
# 
# *****************  Version 3  *****************
# User: Daniel       Date: 98-08-06   Time: 17:11
# Updated in $/Pythondoc
# Added header and footer.
# Added code to compute internal references. This is not very good,
# since it assumes how the output will look. Need to fix that...
# A module's objects now includes subpackages as well.
# Had to fix handling of module references, because of the above
# (a submodule is not a reference any more...)
# Added "locals()" to Method to fix computing of internal references.
# 'Alias' now inherits from 'Variable' to have the 'type' attribute
# appear in the tree.
# Added 'try:...except:' around module import to avoid pythondoc breaking
# when documenting multiple modules.
# 
# *****************  Version 2  *****************
# User: Daniel       Date: 98-07-31   Time: 2:27
# Updated in $/Pythondoc
# Added 'locals()' and 'globals()' functions for computing internal
# crossreferences.
#
