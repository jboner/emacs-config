##############################################################################
# VoiceCode, a programming-by-voice environment
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# (C)2000, National Research Council of Canada
#
##############################################################################

import exceptions, os, posixpath, profile, sys
import types
import traceback
import debug

class EnforcedConstrArg(exceptions.Exception):    
    """Raised by [Object.deep_construct] when the value received for
    an enforced constructor argument differs from the enforced
    default."""

class BadConstrCall(exceptions.Exception):    
    """Raised when a superclasses constructor is called automatically
    with wrong arguments"""
    
class Object:
    """A generic base class for all objects.

    **INSTANCE ATTRIBUTE**

    *none* --

    **CLASS ATTRIBUTE**

    *none* --

    This class implements useful behaviors for generic
    objects, such as:

    - safe attribute setting
    
    - pretty printing (not implemented yet)
    
    - "standard" constructor that:
    
       - automatically invoke constructors of all ancestor classes
       - automatically "declares" valid attributes (for use with safe __setattr__)
       - allows a subclass' constructor to inherit arguments (with default
         values if any) from the constructors of its ancestor clases

    Below is a more detailed description of the various features of the class.

    **SAFE ATTRIBUTE SETTING**

    When getting the value of an attribute, Python will issue an
    *AttributeError* if that attribute doesn't exist. However, it
    doesn't do that when trying to set the value of an inexistant
    attribute.

    The [Object] class defines a safe *__setattr__* method, which
    raises an exception when trying to set the value of an undeclared
    attribute (see below for instructions on how to declare an
    attribute).

    For performance reasons, these safe attribute setting methods are
    only invoked if environment variable *$PY_DEBUG_OBJECT=1*.

    Note that [Object] does not define a safe *__getattr__*
    method because Python already raises an exception when trying to
    get the value of an inexistant attribute.

    Profile tests on NT indicate that:

    - the speed of constructors for Object and non-Object instances
      are the same
    
    - the speed of attribute *gets* is the same for Object and
      non-Object instances
    
    - when *$PY_DEBUG_OBJECT=0*, the performance of attribute *sets*
      is the same for Object and non-Object instances
    
    - when *$PY_DEBUG_OBJECT=1*, attribute *sets* are slower by a
      factor of about 15 for Object instances than for non-Object
      instances


    **"DECLARING" VALID ATTRIBUTES**

    In order to use the safe *__setattr__* method, we need a way to tell it
    what the valid attributes are.

    You can do this simply by setting the attribute directly through
    *__dict__*. You can also use the [decl_attrs] method to declare a
    series of valid attributes with less typing.

    Typically, valid attributes should be set in this way, only in the
    *__init__* method. The rest of the time, they should be set in the
    normal way (or through [init_attrs]), so that the safe __setattr_
    method can intercept access to undeclared attributes..

    Example:
    
       #
       # declare like this inside __init__
       #
       self.__dict__['attr_name'] = None
       self.__dict__['an_other_attr_name'] = None
       
       #
       # or like this if you don't like brainless repetitive typing
       # 
       self.decl_attrs({'attr_name': None, 'an_other_attr_name': None})

       # 
       # set like this the rest of the time
       # 
       self.attr_name = None
       self.an_other_attr_name = None

       # 
       # or like this
       # 
       self.init_attrs({'attr_name': None, 'an_other_attr_name': None})


    Note that you don't need to declare attributes which you pass to [deep_construct]
    through its *attrs_this_class* argument, because [deep_construct]
    will declare them automatically for you.
    

    **STANDARD CONSTRUCTORS**

    The method [deep_construct] can be used to define standard
    constructors with the following properties:
    
    - automatically invokes constructors of all ancestor classes
    
    - automatically "declares" valid attributes (for use with safe *__setattr__*)
    
    - allows a subclass' constructor to inherit arguments (with
      default values if any) from the constructors of its ancestor
      classes

    This has many advantages.
    
    Example:
    
       class Person(Object):
          def __init__(self, name=None, age=None, **args_super):
              etc...
       class Employee(Person):
           def __init__(self, salary=None, **args_super):
               etc...
               
       some_employee = Employee(name='Alain', salary='not enough')

    Note how I was able to feed the *name* to *Employee.__init__*,
    eventhough it doesn't explicitly define that argument. That's
    because *Employee.__init__* "inherited" that argument from
    *Person.__init__*.

    Note also that I didn't have to specify the *age*
    argument. Although it's not obvious from the example, the standard
    constructor automatically sets it to the default *None*
    inherited from *Person.__init__*.

    Below are a series of examples showing how to use [deep_construct]
    to build different standard constructors.

    Note that the file *Admin/python.el* defines some Emacs-Lisp
    macros for writing template code for standard constructors.


    **EXAMPLE 1: Simple case**

    Let's start with a simple case.

    Suppose I want to create a standard class *Person* with attributes
    *name*, *citizenship*, and a standard subclass *Employee* with
    additional attribute *salary*. This would be done as follows.


    Example:
    
       class Person(Object):
          def __init__(self, name, citizenship=None, **args_super):
             self.deep_construct(Person, {'name': name, 'citizenship': citizenship}, args_super)

       class Employee(Person):
           def __init__(self, salary=None, **args_super):
             self.deep_construct(Employee, {'salary': salary}, args_super)

       #
       # This is OK
       #
       some_employee = Employee(name='Alain', salary='not enough')

       #
       # This raises an exception because we don't give a value for
       # compulsory argument name, which is inherited from Person.__init__
       #
       some_other_employee = Employee(salary='not enough')

    Simple no?

    Note how I was able to set attribute *name* through
    *Employee.__init__*, eventhough it is not an argument of that
    constructor.

    Note also that I didn't have to specify a value for
    *citizenship*, because that's only an optional argument inherited from 
    *Person.__init__*.

    Finally, note how construction of *some_other_employee* fails
    because I didn't specify a value for *name*, which is a compulsory
    argument inherited from *Person.__init__*.


    **EXAMPLE 2: Caution regarding compulsory constructor arguments**

    Because compulsory constructor arguments are inherited by
    subclasses, one should be careful not to create too many of
    them. Otherwise, the constructor of subclasses deep in the inheritance
    hierarchy can end up with a great number of compulsory
    arguments. For example, suppose we have a chain of 10 subclasses,
    each adding 3 new compulsory constructor arguments. The
    constructor of the 10th subclass in the chain would end up with 30
    compulsory arguments! The situation becomes even worse in the case
    of multiple inheritance.

    In general, it is better to define constructor arguments to be
    optional, except in cases where the argument has no sensible
    default value. Even in such cases, it's usually pretty safe to use
    *None* as the default value. Remember, this is Python, not C or C++. The
    worst that can happen is that the programer may at some point
    access the argument as though it was not a *None* value, in
    which case he/she will get a nice error message which will allow
    him/her to quickly identify the source of the problem.

    For example, consider the code below.

    Example:

       class Person(Object):
          def __init__(self, age=None):
             self.deep_construct(Person, {'age': age}, args_super)

       #
       # This results in the following error message:
       #
       # Traceback (innermost last):
       #    File "test.py", line 7, in ?
       #      a_person.age = a_person.age + 1
       #  TypeError: bad operand type(s) for +
       #
       a_person = Person()
       a_person.age = a_person.age + 1


        
    To identify the source of the problem, all the user has to do then
    is to print the content of *a_person.age* (using a trace or
    debugger) to figure out that it was left at its default of *None*.

    **EXAMPLE 3: Changing default of an ancestor constructor argument**

    Sometimes, you may want a subclass' constructor to use a default
    value for a constructor argument, which is different from the default
    value defined for the same argument in the constructor of some
    ancestor class.
    
    For example, suppose I know that 99% of the instances of *Person*
    I create will have canadian citizenship. I would like to create a
    standard subclass *MyPerson* whose default value for *citizenship*
    is *'Canadian eh?'*.

    You can do this simply by including *citizenship* and its new
    default value in the argument *new_default* of
    [Object.deep_construct].

    Note that you should use the *new_default* argument only to
    specify default values of arguments defined in some ancestor's
    constructor. To specify the default of an argument defined in the
    current class' constructor, assign a default value in the list of
    arguments to the current class' constructor.
    
    Example:
        
       class Person(Object):
          def __init__(self, name=None, citizenship=None, **args_super):
             self.deep_construct(Person, {'name': name, 'citizenship': citizenship}, args_super)

       class MyPerson(Person):
           #
           # Note that MyPerson.__init__ defines a new argument age. Its
           # default value is defined in MyPerson.__init__'s list of formal
           # arguments, NOT in new_default
           #
           def __init__(self, age=None, **args_super):
             #
             # Because citizenship is an argument of the ancestor Person,
             # we redefine its default using new_default argument.
             #
             self.deep_construct(MyPerson, {'age': age}, args_super, new_default={'citizenship': 'Canadian eh?'})

       #
       # This person is Canadian by default
       #
       some_person = MyPerson(name='Alain')

       #
       # But I can still override that in the call to MyPerson.__init__
       #
       some_other_person = MyPerson(name='Alain', citizenship='US citizen')


    
    **EXAMPLE 4: Enforcing value of an ancestor constructor argument**

    Sometimes, you may want to force the value of an argument defined
    in an ancestor's constructor. This is different from changing the
    default value (as in above example), because here, the user simply
    cannot call the constructor with a value different from the
    enforced value.
 
    In the above example, any instance of *MyPerson* will be Canadian,
    but only by defaul. I can still override that value to create, say
    a US citizens. Now suppose I want to create a class *Canadian*
    where it wasn't possible to override the default value of
    *citizenship='Canadian eh?'*.

    All I have to do is to list *citizenship* and its enforced value
    of *'Canadian eh?'* in the *enforce_value* argument to
    [deep_construct]. If *$PY_DEBUG_OBJECT=1*, this will raise a
    EnforcedConstrArg exception if the programmer calls the
    constructor with a value that's different from the enforced value.

    Note that if the enforced argument happens to be an attribute,
    enforcing its value in the constructor does not prevent the user
    from changing the attribute's value after construction. This is 
    exemplified by the last two lines in the code below.

    
    Example:
        
       class Person(Object):
          def __init__(self, name=None, citizenship=None, **args_super):
             self.deep_construct(Person, {'name': name, 'citizenship': citizenship}, args_super)

       class Canadian(Person):
          def __init__(self, **args_super):
             self.deep_construct(Canadian, {}, args_super, enforce_value={'citizenship': 'Canadian eh?'})

       #
       # This works
       #
       some_canadian = Canadian(name='Alain')

       #
       # This raises an exception because I try to override the enforced
       # value for citizenship.
       #
       pseudo_canadian = Canadian(name='Alain', citizenship='US citizen')

       #
       # But unfortunately, this still manages to create an instance of
       # Canadian with citizenship='US citizen'
       #
       pseudo_canadian = Canadian(name='Alain')
       pseudo_canadian.citizenship = 'US citizen'
        

    **EXAMPLE 5: Arguments which are not attributes**       

    Some of the constructor arguments may not correspond to attributes
    of the class. For example, suppose I want to be able to provide a
    file from which to read initial specification of a *Person* object. I
    don't want this file to be stored as an attribute of the object
    because then the file handle could not be released until the
    object is garbage collected.

    In such cases, all you have to do is to *NOT* pass the argument to 
    [deep_construct]'s *attrs_this_class* argument. 

    Example:
        
       class Person(Object):
          def __init__(self, name=None, citizenship=None, init_file=None, **args_super):
            #
            # Note: deep_construct will not create a init_file attribute
            #       because it's not in its attrs_this_class argument.
            #
            self.deep_construct(Person, {'name': name, 'citizenship': citizenship}, args_super)
            if init_file: self.init_from_file(init_file)

       # Person doesn't get a init_file attribute
       a_person = Person(init_file=open('C:/temp.txt', 'r'))
       try:
           x = a_person.init_file
       except:
           print 'See, a_person has no attribute called *init_file*'


    **EXAMPLE 6: Private attributes**

    Some attributes are private, that is they are not supposed to be
    manipulated directly by the user.

    In the above example, suppose I want to be able to determine if the
    initialisation file has changed since the last time I initialised
    the instance of *Person*. I would do that by adding an attribute
    *date_last_read* which stores the date at which the instance was
    last initialised from file. Obviously, I don't want the programmer
    to set this value through constructor arguments.

    In this case, all I have to do is to exclude *date_last_read* from
    *Person.__init__*'s list of arguments, and declare it directly in
    *__init__* using [decl_attrs].

    Example:
        
       class Person(Object):
          #
          # Note how date_last_read is absent from list of arguments
          #
          def __init__(self, name=None, citizenship=None, init_file=None, **args_super):
             #
             # Instead, date_last_read attribute is declared directly here.
             #
             self.decl_attrs({'date_last_read': localtime()})
             #
             # No need to pass date_last_read to deep_construct, because it
             # was already declared above.
             # 
             self.deep_construct(Person, {'name': name, 'citizenship': citizenship}, args_super)
             if init_file: self.init_from_file(init_file)
       #
       # This works, and date_last_read ends up being set at current time.
       # 
       a_person = Person(init_file=FileObject(path='C:/person.dat'))
       #
       # This raises an exception because date_last_read is not a constructor
       # argument
       #
       a_person = Person(init_file=FileObject(path='C:/person.dat'), date_last_read=231233)

    **EXAMPLE 7: Doing other initialisation besides attribute setting**

    Often, a constructor needs to do more than just set attribute
    values. To do this, simply add code after the call to [deep_construct].

    In the previous example, we added code for conditionally calling
    *self.init_from_file(init_file)* after [deep_construct].


    **EXAMPLE 8: Subclassing from non-standard classes**

    Sometimes you will want to create a standard class which is a
    subclass of a non-standard class, that is, a class that doesn't
    have a standard constructor.

    In such cases, you would simply add the non-standard class
    to the *exclude_bases* argument of [deep_construct], and invoke
    the non-standard constructor manually in *__init__*.

    For example, suppose I want *Person* to also be a subclass of
    *AnimatedCharacter*, but *AnimatedCharacter.__init__* is not a
    standard constructor. 


    Example:
    
       class AnimatedCharacter():
          def __init__(self, animation_file, frames_per_sec=40):
              etc...
              
       class Person(Object):
          def __init__(self, name=None, citizenship=None, **args_super):
              etc...

       class AnimatedPerson(Person, AnimatedCharacter)
          def __init__(self, , animation_file, frames_per_sec=40, **args_super):
             self.deep_construct(AnimatedPerson, {'animation_file': animation_file, 'frames_per_sec': frames_per_sec}, args_super, exclude_bases={AnimatedCharacter: 1})
             AnimatedCharacter.__init__(self, animation_file, frames_per_sec=frames_per_sec)

       an_animated_person = AnimatedPerson(name='Alain', animation_file='C:/People/Alain.dat')


    Note that the only reason we have to exclude *AnimatedCharacter*
    from the list of automatically built superclasses, is that
    *AnimatedCharacter.__init__* doesn't have a catch-all argument
    *\*\*args_super*. If *AnimatedCharacter.__init__* DID have such a
    catch-all argument, we wouldn't have to exclude it, even if it
    didn't make use of the other aspects of the standard constructor.

    Note also that I still get all of the benefits from the standard
    constructor of *Person*, i.e. I can inherit the arguments of
    *Person.__init__* without additional work.

    However, if I want to inherit the arguments of
    *AnimatedCharacter.__init__*, I have to repeat them in the list of
    arguments for *AnimatedPerson.__init__* and then pass them to
    *AnimatedCharacter.__init__* manually.

    Note also that in *exclude_bases*, I specify the class object
    itself, not the name of the classe, that is:

    *exclude_bases = {AnimatedPerson: 1}*

    as opposed to

    *exclude_bases = {'AnimatedPerson': 1}*
        
    .. [decl_attrs] file:///./Object.Object.html#Object.Object.decl_attrs
    .. [init_attrs] file:///./Object.Object.html#Object.Object.init_attrs
    .. [deep_construct] file:///./Object.Object.html#Object.Object.deep_construct"""


    #
    # Safe __setattr__ method is defined in file 'safe_setattr.py'.
    # Load it only if environment variable PY_DEBUG_OBJECT=1
    #
    if (os.environ.has_key('PY_DEBUG_OBJECT') and (os.environ['PY_DEBUG_OBJECT'] != '0')):
        code_file = posixpath.expandvars('$VCODE_HOME' + os.sep + 'Mediator' + os.sep + 'safe_setattr.py')
        execfile(code_file)


    def __init__(self, **args_super):
        pass

    def deep_construct(self, this_class, attrs_this_class, args_super, new_default={}, enforce_value={}, exclude_bases={}):
        """Build an instance of a class.

        Basically, this method:
        - declares and initialise all attributes listed in *attrs_this_class*
        - invokes the *__init__* of all superclasses (with the exclusion of those listed in *exclude_bases*), passing them arguments in *args_super*

        *CLASS* this_class -- Class that we want to build. This is a
         class object as opposed to the name of a class. Constructors
         of immediate superclasses of *this_class* are called
         automatically, except if they are listed in *{CLASS: 1}
         exclude_bases*.

        *{STR: ANY}* attrs_this_class -- New attributes (and their
         values) defined by class *this_class*. The keys are the names
         of the attributes and the values are the values of the
         attributes (either default values or values passed to
         *this_class.__init__*). An attribute with the appropriate
         name will be declared automatically and initialsed to the
         value specified in *attrs_this_class*.
        
        *{STR: ANY}* args_super -- Arguments received by
        *this_class.__init__* but not recognised by it. These are
        assumed to be arguments defined in the *__init__* of some
        ancestor class and are just passed up the construction
        chain. Keys of *args_super* correspond to the names of the
        arguments and the values corresponds to the values received
        for them by *this_class.__init__*

        *{STR: ANY}* new_default={} -- Used to change the default
         value of an ancestor constructor argument. In other words, if
         *this_class.__init__* was called without specifying a value
         for an argument that's listed in *new_default*, the default
         value defined in *new_default* will be used instead of
         whatever default might be defined in the constructor
         of an ancestor class. However, if the constructor was called
         WITH a specific value for that argument, that specific value
         will be used instead of both the defaults defined in
         *new_default* and the constructor of ancestor classes. Keys
         of *new_default* correspond to argument names, and values
         correspond to the new default values. If you don't specify a
         value of *new_default*, it defaults to *{}*, which means that
         the defaults of none of the ancestor constructor arguments
         are redefined by *this_class*.
        
        *{STR: ANY}* enforce_value={} -- Lists of arguments with
         enforced values. If the constructor is called with a value
         for an argument that is different from the value specified
         for it in *enforce_value*, then an [EnforcedConstrArg]
         exception will be raised. Also, if the constructor is called
         without specifying a value for a particular argument, then
         the value defined in *enforce_value* (if it exists) will be
         used instead of whatever default might be defined in an
         ancestor class. Keys of *enforce_value* correspond to
         argument names and values correspond to the enforced
         values. If you don't specify a value for *enforce_value*, it
         defaults to *{}*, which means that *this_class.__init__* does
         not enforce the value of any argument.
          
                 
       *{CLASS: BOOL}* exclude_bases -- Immediate base classes whose
        constructors should not be called automatically. If an
        immediate superclass of *this_class* is listed in
        *exclude_bases*, then we don't automatically call its
        constructor. It is assumed that the programmer will call the
        constructor manually in *this_class.__init__*. If you do not
        specify a value for *exclude_bases*, it will default to *{}*,
        which means that the constructor of all immediate super
        classes will be called automatically.


        .. [EnforcedConstrArg] file:///./Object.EnforcedConstrArg.html
        .. [Object] file:///./Object.Object.html"""

#        debug.trace('Object.deep_construct', 'this_class=%s' % this_class)
        
        #
        # Redefine the default value of some ancestor constructor
        # arguments.
        #
        for an_arg in new_default.items():
          if not args_super.has_key(an_arg[0]):
              #
              # this_class.__init__  called without a value for the argument.
              # Use the default value provided by this_class.__init__,  instead
              # of waiting for ancestor class to set it to its own default
              # value.
              #
              args_super[an_arg[0]] = an_arg[1]

        #
        # Enforce values of some superclass arguments
        #
        for an_arg in enforce_value.items():
            if args_super.has_key(an_arg[0]):
                #
                # this_class.__init__ called with a value for this argument
                #
                if args_super[an_arg[0]] != an_arg[1]:
                    #
                    # The value provided is not the same as the enforced one.
                    # Raise an exception
                    #
                    raise EnforcedConstrArg('The value of argument %s in %s.__init__ is enforced at \'%s\', and cannot be changed.' % (an_arg[0], repr(this_class), an_arg[1]))
            else:
                #
                # this_class.__init__ called without a value for this argument.
                # Set it to the enforced value
                #
                args_super[an_arg[0]] = an_arg[1]            

        #
        # Declare and initialise new attributes which are defined by
        # this_class.
        #
        # Note: We do this BEFORE invoking constructor of superclasses
        #       because the constructors of superclasses might invoke
        #       some virtual method whose overriden implementation
        #       assumes that the attributes are defined.
        #
        #       For example, this happens often when the superclass
        #       constructor invokes a factory method to build one of 
        #       its members. The concrete implementation of that
        #       factory method often needs to access attributes
        #       of the subclass.
        #
        self.decl_attrs(attrs_this_class)


        #
        # Invoke constructor of the superclasses, feeding them arguments
        # not recognised by this_class.__init__
        #
        for a_base in this_class.__bases__:
            if not exclude_bases.has_key(a_base):
                try:
                    apply(a_base.__init__, [self], args_super)
                except TypeError:
                    msg = "TypeError while initializing base %s of class %s\n" \
                        % (str(a_base), str(this_class))
                    sys.stderr.write(msg)
                    msg = "keyword arguments were: %s\n" % str(args_super)
                    sys.stderr.write(msg)
                    raise
                    
    
    def decl_attrs(self, attrs):
        """Define new attributes for *self*

        Attributes are directly through self.__dict__, thus bypassing safe
        __setattr__.

        **INPUTS**

        *{STR: ANY}* attrs -- dictionary with attribute name as the keys and
         initial values as the values.

        **OUTPUTS**

        *none* -- 
        """
        for an_attr_def in attrs.items():
            self.__dict__[an_attr_def[0]] = an_attr_def[1]        

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
        for key, value in attrs.items():
            setattr(self, key, value)
       
    def possibly_init_attrs(self, attrs):
        """Initialises existing attributes, unless those attributes
        already exist
        
        **INPUTS**

        *{STR: ANY}* attrs -- dictionary with attribute name as the keys and
         default values as the values.

        **OUTPUTS**

        *none* -- 
        """
        for key, value in attrs.items():
            if not self.__dict__.has_key(key):
                setattr(self, key, value)

# DCF: ChildObject is obsolete.  Use OwnerObject instead.
class ChildObject(Object):
    """a subclass of Object which contains a reference to its parent
    (e.g. SourceBuff to its parent AppState).  Since the parent contains
    a reference to its children, this creates circular references, which
    prevent the parent from automatically reaching a reference count of zero,
    and being deleted.  To avoid this problem, the parent object should
    call the cleanup method of the child before removing its reference
    to the child.

    **INSTANCE ATTRIBUTE**

    *none* --

    **CLASS ATTRIBUTE**

    *none* --
    """
    def cleanup(self):
        """method to cleanup circular references by cleaning up 
        any children, and then removing the reference to the parent

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
# the parent may be called something more specific than "parent", so we
# can't define a generic cleanup here.  The specific subclasses will
# have to do that.  Also, the child object may have children of its own,
# in which case it should call their cleanup function first.
        pass

class OwnerObject(Object):
    """a subclass of Object which contains a reference to its parent, or
    which owns references to some of its children.  Any object which 
    contains a reference to its parent (e.g. SourceBuff to its 
    parent AppState) must have exactly one owner.  
    Since the parent contains a reference to its children, this creates 
    circular references, which prevent the parent from automatically 
    reaching a reference count of zero, and being deleted.  To avoid this 
    problem, the parent object should call the cleanup method of the 
    child before removing its reference to the child.

    **INSTANCE ATTRIBUTE**

    *[STR] owned_objects* -- list of attribute names of objects owned 
    by self (whose cleanup method should be called when this object's 
    owner calls this object's cleanup method).  Note: if the attribute
    corresponding to an element of owned_objects is a list, cleanup will
    be called on the elements of the list.  Similarly, if the
    corresponding attribute is a map, cleanup will be
    called on the values of the map.

    *STR parent_name* -- name of attribute containing reference to the parent
    object, or None if no reference to parent

    **CLASS ATTRIBUTE**

    *none* --
    """
    def __init__(self, **attrs):
        self.possibly_init_attrs({'parent_name': None,
                                  'grandparents': [],
                                  'owned_objects': []})
        self.deep_construct(OwnerObject,
                            {},
                            attrs)

    def owned_by(self):
        """returns the name of the parent attribute

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- name of the parent, or None if none
        """
        return self.parent_name

    def name_parent(self, parent = None):
        """specify the name of the attribute containing a reference to 
        this object's parent.

        **INPUTS**

        *STR parent* -- name of the parent, or None if none

        **OUTPUTS**

        *none*
        """
        if self.parent_name != None:
            raise RuntimeError('OwnerObject named a second parent')
        self.parent_name = parent

    def add_grandparent(self, grandparent):
        """specify the name of the attribute containing a reference to 
        this object's grandparent (or great-grandparent, etc.)
        (Actually, any other reference which needs to be del'ed but not
        cleaned up)

        **INPUTS**

        *STR grandparent* -- names of owned attributes

        **OUTPUTS**

        *none*
        """
        self.grandparents.append(grandparent)


    def add_owned(self, owned):
        """append a new attribute name to the list of owned objects

        **INPUTS**

        *STR owned* -- names of owned attributes

        **OUTPUTS**

        *none*
        """
        self.owned_objects.append(owned)


    def add_owned_list(self, owned):
        """append new attribute names to the list of owned objects

        **INPUTS**

        *[STR] owned* -- names of owned attributes

        **OUTPUTS**

        *none*
        """
        self.owned_objects.extend(owned)

    def remove_other_references(self):
        """additional cleanup to ensure that this object's references to
        its owned objects are the last remaining references

        **NOTE:** subclasses must call their parent class's 
        remove_other_references method, after performing their own duties.
        Also, a class inheriting from two OwnerObject classes MUST
        define remove_other_references and call both subclasses'
        versions

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
# subclasses must call their parent class's remove_other_references
# method, after performing their own duties
        pass

    def _cleanup_object(self, object):
        """attempt to call cleanup on object

        **INPUTS***

        *OwnerObject object* -- note: class of object is the expected
        class, but _cleanup_object doesn't assume this is correct, nor
        does it check whether the object is a subclass of
        OwnerObject, only that it is a class instance and that it
        has a cleanup attribute

        **OUTPUTS**

        *STR* -- reason for error (or None if no error).
        """

        if object == None:
            return None
        if type(object) == types.ListType:
            for i in range(object):
                error_msg = self._cleanup_object(object[i])
                if error_msg != None:
                    error_msg = 'element %d of ' % i
                    return error_msg
            return None
        elif type(object) == types.DictType:
            sorted = object.keys()
            sorted.sort()
            for key in sorted:
                error_msg = self._cleanup_object(object[key])
                if error_msg != None:
                    error_msg = 'value for key %s of ' % str(key)
                    return error_msg
            return None

        if (type(object) != types.InstanceType and
           not debug.isinstance_of_some_class(object)):
            return 'because it is not an object, but has type %s' % (type(object))
        try:
            object.cleanup
        except AttributeError:
            return 'because it does not have a cleanup method'
        try:
            object.cleanup()
        except:
            traceback.print_exc()
            return 'because its cleanup method threw an exception'
        
    def cleanup(self):
        """method to cleanup circular references by cleaning up 
        any children, and then removing the reference to the parent

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
#        debug.trace_call_stack('OwnerObject.cleanup')
        self.remove_other_references()
        debug.trace('OwnerObject.cleanup', 'after remove_other_references')
    
        reversed_names = self.owned_objects
        reversed_names.reverse()
        msg_prefix = 'Warning: while cleaning up %s,\nunable to cleanup ' \
            % repr(self)
        for name in reversed_names:
            if self.__dict__.has_key(name):
                attribute = self.__dict__[name]
                debug.trace('OwnerObject.cleanup', 
                    'cleanup %s with value %s' % (name, repr(attribute)))
                if attribute == None:
                    continue
                if type(attribute) == types.ListType:
                    rr = range(len(attribute))
                    rr.reverse()
                    for i in rr:
                        error_msg = self._cleanup_object(attribute[i])
                        if error_msg != None:
                            error_msg = 'element %d of attribute %s\n%s\n' \
                                % (i, name, error_msg)
                            debug.critical_warning(msg_prefix + error_msg)
                        else:
                            del attribute[i]
                elif type(attribute) == types.DictType:
                    sorted = attribute.keys()
                    sorted.sort()
                    for key in sorted:
                        error_msg = self._cleanup_object(attribute[key])
                        if error_msg != None:
                            error_msg = 'value for key %s of attribute %s\n%s\n' \
                                % (str(key), name, error_msg)
                            debug.critical_warning(msg_prefix + error_msg)
                        else:
                            del attribute[key]
                elif (type(attribute) == types.InstanceType or 
                      debug.isinstance_of_some_class(attribute)):
                    error_msg = self._cleanup_object(attribute)
                    if error_msg != None:
                        error_msg = 'attribute %s\n%s\n' \
                            % (name, error_msg)
                        debug.critical_warning(msg_prefix + error_msg)
                    else:
                        del self.__dict__[name]
                else:
                    error_msg = 'because it is not an object, ' \
                        + 'but has type %s' % (type(attribute))
                    error_msg = 'attribute %s\n%s\n' \
                        % (name, error_msg)
                    debug.critical_warning(msg_prefix + error_msg)
                    del self.__dict__[name]

            else:
                error_msg = 'because the attribute does not exist\n'
                error_msg = 'attribute %s\n%s' \
                    % (name, error_msg)
                debug.critical_warning(msg_prefix + error_msg)
        
        for grandparent in self.grandparents:
            if self.__dict__.has_key(grandparent):
                del self.__dict__[grandparent]

        if self.parent_name != None \
                and self.__dict__.has_key(self.parent_name):
            del self.__dict__[self.parent_name]
        debug.trace('OwnerObject.cleanup', 'cleanup of %s finished' % repr(self))
          

##############################################################################
# The remaining code is just for profile testing purposes
# For some reason, profil tests can't be run using Admin/test.py
# The must be run with:
#    python Object.py
##############################################################################


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

        

def prof_test():
    sys.stdout.write('\n$PY_DEBUG_OBJECT is: ')
    if (os.environ.has_key('PY_DEBUG_OBJECT')):
        sys.stdout.write(os.environ['PY_DEBUG_OBJECT'])
    else:
        sys.stdout.write('not defined')
    sys.stdout.write("\n\n")

    sys.stdout.write("Profiling speed of Object constructor/get/set.\n\n")
    profile.run("profObject(1000)")
#    profObject(1000)


if (__name__ == "__main__"):
    prof_test()
                        

