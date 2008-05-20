
# This symbol is here because it is homophonic with auto_test. Just checking
# to make sure that symbol match works with homophonic symbols.
autoTst = 0

import auto_test, exceptions, os, posixpath, profile, sys

class Object:
    """A base class for all VoiceCode objects

    This class implements various useful behaviors for generic
    objects, such as:

    - safe attribute setting
    - deep constructor
    - pretty printing???
    

    **SAFE ATTRIBUTE SETTING***

    When getting the value of an attribute, Python will issue an
    *AttributeError* if that attribute doesn't exist. However, it
    doesn't do that when trying to set the value of an inexistant
    attribute.

    The [Object] class defines a safe *__setattr__* method, which raise an
    exception when trying to set the value of an inexistant attribute.

    For performance reasons, these safe attribute setting methods are
    only invoked if environment variable *$PY_DEBUG_OBJECT=1*.

    Note that this class does not define a safe *__getattr__*
    method because Python already raises an exception when trying to
    get the value of an inexistant attribute.

    Profile tests on NT indicate that:

    - the speed of constructors for Object and non-Object instances are the same
    - the speed of attribute *gets* is the same for Object and non-Object instances
    - when *$PY_DEBUG_OBJECT=0*, the performance of attribute *sets* is the same for Object and non-Object instances
    - when *$PY_DEBUG_OBJECT=1*, attribute *sets* are slower by a factor of about 15 for Object instances than for non-Object instances


    **DEEP CONSTRUCTORS**

    By default, when constructing an instance of a class, Python does
    not call the constructor of all the ancestor classes. This means
    that the constructor of each class must set the values of the
    attributes defined by all of its ancestors, otherwise reading the
    value of an ancestor attribute (or setting it when
    *PY_DEBUG_OBJECT=1*) results in an *AttributeError*. This also
    means that subclasses cannot inherit default values of ancestor
    attributes from their ancestor classes.
    
    The [Object] class defines a method [deep_construct], which can be
    used to create "standard" constructors with interesting properties:

    - it automatically invoke the constructor of any superclass that has a "standard" constructor
    - it inherits default attribute values from ancestor classes
    - you can call it without arguments to obtain a consistent default instance
    - you can override the default value of any attribute (including ones defined by ancestor classe) simply by passing a named argument to the constructor
    - you can leave the value of any attribute (including ones defined by ancestor classe) at its default value, simply by not passing a named argument for that attribute

    Below is a template for such a standard constructor.

    Example:

       class AClass(StdClass1, ..., StdClassN, NonStdClass1, ..., NonStdClassK):
          def __init__(self, attr1=val1, ..., attrN=valN, **attrs):
              
             self.deep_construct(AClass, {'attr1': val1, ..., 'attrN': valN}, attrs, exclude_bases={NonStdClass1: 1, ..., NonStdClassK: 1})
             
             <Code for explicitely calling NonStdClass1.__init__, ..., NonStdClassK.__init__ with appropriate arguments>
             
             <Any other initialisation that can't be done automatically
              through *self.deep_construct*>

    Here, *StdClass1, ..., StdClassN* are "standard" classes, that is
    classes that have standard constructor. The constructor of those
    classes can be invoked automatically because it doesn't have
    any compulsory arguments.

    *NonStdClass1, ..., NonStdClassK* are "non-standard" classes,
     i.e. classes whose constructor has some compulsory arguments. The
     constructor of those non-standard classes can therefore not be
     called automatically (which is why they are set in the
     *exclude_bases* argument and their constructor is explicitely
     called after the call to *self.deep_construct*)
    
    Attributes *attr1, ..., attrN* are the new attributes defined by
    *AClass*, and *val1, ..., valN* are their default values.

    The argument *{STR: ANY} **attrs* collects any named arguments fed
    to the constructor which don't correspond to an attribute defined
    by this class. These will be used to set the values of attributes
    defined in ancestor classes.
                       
    Note that the file *Admin/python.el* contains an Emacs macro
    *py-obclass* which automatically types this kind of template code
    for a class and its constructor

    **INSTANCE ATTRIBUTE**

    *none* --

    **CLASS ATTRIBUTE**

    *none* --

    .. [def_attrs] file:///./Object.Object.html#Object.Object.def_attrs
    .. [init_attrs] file:///Object.Object.html#Object.Object.init_attrs"""


    #
    # Safe __setattr__ method is defined in file 'safe_setattr.py'.
    # Load it only if environment variable PY_DEBUG_OBJECT=1
    #
    if (os.environ.has_key('PY_DEBUG_OBJECT') and (os.environ['PY_DEBUG_OBJECT'] != '0')):
        code_file = posixpath.expandvars('$VCODE_HOME' + os.sep + 'Mediator' + os.sep + 'safe_setattr.py')
        execfile(code_file)


    def __init__(self):
        pass

    def deep_construct(self, this_class , attrs_this_class, attrs_superclasses, exclude_bases={}):
        """Build an instance of a class.

        Make *[Object] self* into an instance of class *CLASS this_class*.

        Automatically call constructors of superclasses of
        *this_class* (except for classes listed in *{CLASS: 1}
        exclude_bases*). Constructors are called with no arguments.
        
        Set attributes to the values listed in *{STR: ANY}
        attrs_this_class* and *{STR: ANY} attrs_cupserclasses*. These
        attributes are set even if they do not exist in
        *self.__dict__*.
        
        listed in *exclude_bases* argument). These constructors are
        called with no arguments.

        .. [Object] file:///./Object.Object.html"""

        for a_base in this_class.__bases__:
            if not exclude_bases.has_key(a_base):
                a_base.__init__(self)

        for an_attr_def in attrs_this_class.items():
            self.__dict__[an_attr_def[0]] = an_attr_def[1]        

        for an_attr_def in attrs_superclasses.items():
            self.__dict__[an_attr_def[0]] = an_attr_def[1]        

    
#      def def_attrs(self, attrs):
#          """Define new attributes for *self*

#          Attributes are set even if they do not exist in
#          *self.__dict__*.

#          **INPUTS**

#          *{STR: ANY}* attrs -- dictionary with attribute name as the keys and
#           default values as the values.

#          **OUTPUTS**

#          *none* -- 
#          """
#          for an_attr_def in attrs.items():
#              self.__dict__[an_attr_def[0]] = an_attr_def[1]        

    def init_attrs(self, attrs):
        """Initialises existing attributes

        Attributes are only set if they already exist in
         *self.__dict__*. Otherwise, an *AttributeError* exception is
         raised (provided PY_DEBUG_OBJECT=1).
        
        **INPUTS**

        *{STR: ANY}* attrs -- dictionary with attribute name as the keys and
         default values as the values.

        **OUTPUTS**

        *none* -- 
        """
        for an_attr_init in attrs.items():
            setattr(self, an_attr_init[0], an_attr_init[1])


    
class SmallObject(Object):
    """A test object with few attributes

    This class is used to profile the performance of access methods
    when accessing attribute from a small Object subclass.

    **INSTANCE ATTRIBUTE**

    *STR* name --
    *INT* age --

    **CLASS ATTRIBUTE**
    
    *none* -- 
    """
    def __init__(self, name="Alain", age=36):
        """Constructor

        **INPUTS**

        *STR* name="Alain" -- undocumented 

        *INT* age=36 -- undocumented 


        **OUTPUTS**

        *none* -- 
        """
        self.__dict__['name'] = name
        self.__dict__['age'] = age

class LargeObject(Object):
    """A test object with many attributes

    This class is used to profile the performance of access methods
    when accessing attributes of a large Object subclass.

    **INSTANCE ATTRIBUTE**

    *STR* name --
    *INT* age --
    *INT* attr1, ..., attr20 --

    **CLASS ATTRIBUTE**

    *none* -- 
    """

    def __init__(self, name="Alain", age="36"):
        """Constructor

        **INPUTS**

        *ANY* name="Alain" -- undocumented 

        *ANY* age="36" -- undocumented 


        **OUTPUTS**

        *none* -- 
        """
        self.__dict__['name'] = name
        self.__dict__['age'] = age
        self.__dict__['attr1'] = 1
        self.__dict__['attr2'] = 1
        self.__dict__['attr3'] = 1
        self.__dict__['attr4'] = 1
        self.__dict__['attr5'] = 1
        self.__dict__['attr6'] = 1
        self.__dict__['attr7'] = 1
        self.__dict__['attr8'] = 1
        self.__dict__['attr9'] = 1
        self.__dict__['attr10'] = 1
        self.__dict__['attr11'] = 1
        self.__dict__['attr12'] = 1
        self.__dict__['attr13'] = 1
        self.__dict__['attr14'] = 1
        self.__dict__['attr15'] = 1
        self.__dict__['attr16'] = 1
        self.__dict__['attr17'] = 1
        self.__dict__['attr18'] = 1
        self.__dict__['attr19'] = 1
        self.__dict__['attr20'] = 1        

class SmallNonObject:
    """A \"raw\" test object with many attributes.

    This class is used to profile the performance of attribute access
    methods for large objects which are not subclasses of Object.

    **INSTANCE ATTRIBUTE**

    *STR* name --
    *INT* age --

    **CLASS ATTRIBUTE**

    *none* -- 
    """

    def __init__(self, name="Alain", age=36):
        """Constructor

        **INPUTS**

        *STR* name="Alain" -- undocumented 

        *INT* age=36 -- undocumented 


        **OUTPUTS**

        *none* -- 
        """
        self.__dict__['name'] = 'Alain'
        self.__dict__['age'] = 36

class LargeNonObject:
    """A \"raw\" test object with many attributes.

    This class is used to profile the performance of attribute access
    methods for large objects which are not subclasses of Object.

    **INSTANCE ATTRIBUTE**

    *STR* name --
    *INT* age --
    *INT* attr1, ..., attr20 --

    **CLASS ATTRIBUTE**

    *none* -- 
    """

    def __init__(self, name="Alain", age=36):
        """Constructor

        **INPUTS**

        *STR* name="Alain" -- undocumented 

        *INT* age=36 -- undocumented 


        **OUTPUTS**

        *none* -- 
        """
        self.__dict__['name'] = name
        self.__dict__['age'] = age
        self.__dict__['attr1'] = 1
        self.__dict__['attr2'] = 1
        self.__dict__['attr3'] = 1
        self.__dict__['attr4'] = 1
        self.__dict__['attr5'] = 1
        self.__dict__['attr6'] = 1
        self.__dict__['attr7'] = 1
        self.__dict__['attr8'] = 1
        self.__dict__['attr9'] = 1
        self.__dict__['attr10'] = 1
        self.__dict__['attr11'] = 1
        self.__dict__['attr12'] = 1
        self.__dict__['attr13'] = 1
        self.__dict__['attr14'] = 1
        self.__dict__['attr15'] = 1
        self.__dict__['attr16'] = 1
        self.__dict__['attr17'] = 1
        self.__dict__['attr18'] = 1
        self.__dict__['attr19'] = 1
        self.__dict__['attr20'] = 1
        
def profConstrSmallObj(num_times):
    """Profile performance of constructor

    This method profiles the performance of constructors for classes
    that *are* subclasses of Object and have *few* attributes

    **INPUTS**

    *INT* num_times -- number of times to invoke the constructor 

    **OUTPUTS**

    *none* -- 
    """
    for index in range(num_times):
        obj = SmallObject(name='Janet', age=42)

def profConstrLargeObj(num_times):
    """Profile performance of constructor

    This method profiles the performance of constructors for classes
    that *are* subclasses of Object and have *many* attributes

    **INPUTS**

    *INT* num_times -- number of times to invoke the constructor

    **OUTPUTS**

    *none* -- 
    """
    for index in range(num_times):
        obj = LargeObject(name='Janet', age=42)

def profConstrSmallNonObj(num_times):
    """Profile performance of constructor

    This method profiles the performance of constructors for classes
    that *are not* subclasses of Object and have *few* attributes

    **INPUTS**

    *INT* num_times -- number of times to invoke the constructor 

    **OUTPUTS**

    *none* -- 
    """
    for index in range(num_times):
        obj = SmallNonObject(name='Janet', age=42)

def profConstrLargeNonObj(num_times):
    """Profile performance of constructor

    This method profiles the performance of constructors for classes
    that *are not* subclasses of Object and have *many* attributes

    **INPUTS**

    *INT* num_times -- number of times to invoke the constructor 

    **OUTPUTS**

    *none* -- 
    """
    for index in range(num_times):
        obj = LargeNonObject(name='Janet', age=42)


def profGetSmallObj(num_times):
    """Profile performance of attribute *get*

    This method profiles the performance of attribute *get*  classes
    that *are* subclasses of Object and have *few* attributes

    **INPUTS**

    *INT* num_times -- number of times to invoke the get 

    **OUTPUTS**

    *none* -- 
    """

    obj = SmallObject(name='Janet', age=42)
    for index in range(num_times):
        her_age = obj.age


def profGetLargeObj(num_times):
    """Profile performance of attribute *get*

    This method profiles the performance of attribute *get*  classes
    that *are* subclasses of Object and have *many* attributes

    **INPUTS**

    *INT* num_times -- number of times to invoke the get 

    **OUTPUTS**

    *none* -- 
    """

    obj = LargeObject(name='Janet', age=42)
    for index in range(num_times):
        her_age = obj.age

def profGetSmallNonObj(num_times):
    """Profile performance of attribute *get*

    This method profiles the performance of attribute *get*  classes
    that *are not* subclasses of Object and have *few* attributes

    **INPUTS**

    *INT* num_times -- number of times to invoke the get 

    **OUTPUTS**

    *none* -- 
    """

    obj = SmallNonObject(name='Janet', age=42)
    for index in range(num_times):
        her_age = obj.age

def profGetLargeNonObj(num_times):
    """Profile performance of attribute *get*

    This method profiles the performance of attribute *get*  classes
    that *are not* subclasses of Object and have *many* attributes

    **INPUTS**

    *INT* num_times -- number of times to invoke the get 

    **OUTPUTS**

    *none* -- 
    """

    obj = LargeNonObject(name='Janet', age=42)
    for index in range(num_times):
        her_age = obj.age

def profSetSmallObj(num_times):
    """Profile performance of attribute *get*

    This method profiles the performance of attribute *set*  classes
    that *are* subclasses of Object and have *few* attributes

    **INPUTS**

    *INT* num_times -- number of times to invoke the get 

    **OUTPUTS**

    *none* -- 
    """

    obj = SmallObject(name='Janet', age=42)
    for index in range(num_times):
        obj.age = 1

def profSetLargeObj(num_times):
    """Profile performance of attribute *get*

    This method profiles the performance of attribute *set*  classes
    that *are* subclasses of Object and have *many* attributes

    **INPUTS**

    *INT* num_times -- number of times to invoke the get 

    **OUTPUTS**

    *none* -- 
    """

    obj = LargeObject(name='Janet', age=42)
    for index in range(num_times):
        obj.age = 1

def profSetSmallNonObj(num_times):
    """Profile performance of attribute *get*

    This method profiles the performance of attribute *set*  classes
    that *are not* subclasses of Object and have *few* attributes

    **INPUTS**

    *INT* num_times -- number of times to invoke the get 

    **OUTPUTS**

    *none* -- 
    """

    obj = SmallNonObject(name='Janet', age=42)
    for index in range(num_times):
        obj.age = 1                

def profSetLargeNonObj(num_times):
    """Profile performance of attribute *get*

    This method profiles the performance of attribute *set*  classes
    that *are not* subclasses of Object and have *many* attributes

    **INPUTS**

    *INT* num_times -- number of times to invoke the get 

    **OUTPUTS**

    *none* -- 
    """

    obj = LargeNonObject(name='Janet', age=42)
    for index in range(num_times):
        obj.age = 1                


def profObject(num_times):
    """Profile the performance of the Object class

    **INPUTS**

    *INT* num_times -- number of times to carry out the various tests


    **OUTPUTS**

    *none* -- 
    """
    profConstrSmallObj(num_times)
    profConstrLargeObj(num_times)
    profConstrSmallNonObj(num_times)
    profConstrLargeNonObj(num_times)
    profGetSmallObj(num_times)
    profGetLargeObj(num_times)
    profGetSmallNonObj(num_times)
    profGetLargeNonObj(num_times)	
    profSetSmallObj(num_times)
    profSetLargeObj(num_times)
    profSetSmallNonObj(num_times)
    profSetLargeNonObj(num_times)

def try_attribute(obj, name, operation):
    """Test setting/getting attributes

    **INPUTS**

    *ANY* obj -- object on which we will get/set attributes 

    *STR* name -- name of attribute to get/set 

    *STR* operation -- *'get'* or *'set'*

    **OUTPUTS**

    *none* -- 
    """
    sys.stdout.write("\nTrying to %s the value of attribute '%s'\n   -> " % (operation, name))
    if (operation == 'set'):
        code = "obj." + name + " = '999'"
    else:
        code = "x = obj." + name
    x = 0
    try:
        exec(code)
    except AttributeError, exc:
        sys.stdout.write("Caught AttributeError exception: '%s'" % [exc.__dict__])
    else:
        sys.stdout.write("Caught NO AttributeError exception. ")
        str = "obj.%s=%s, x=%s" % (name, obj.name, x)
        sys.stdout.write(str)
    sys.stdout.write("\n\n")
        

def prof_test():
    sys.stdout.write('\n$PY_DEBUG_OBJECT is: ')
    if (os.environ.has_key('PY_DEBUG_OBJECT')):
        sys.stdout.write(os.environ['PY_DEBUG_OBJECT'])
    else:
        sys.stdout.write('not defined')
    sys.stdout.write("\n\n")

    sys.stdout.write("Profiling speed of Object constructor/get/set.\n\n")
    profile.run("profObject(1000)")
#    profile.run("profObject(5000)")
#    profObject(1000)

def self_test():
    obj = SmallObject()

    sys.stdout.write("Testing exceptions for get/set\n\n")
    try_attribute(obj, 'name', 'get')
    try_attribute(obj, 'name', 'set')
    try_attribute(obj, 'nonexistant', 'get')
    try_attribute(obj, 'nonexistant', 'set')

auto_test.add_test('Object', self_test, 'self-test for Object.py')

if (__name__ == "__main__"):
    self_test()
    prof_test()
			

