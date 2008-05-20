from __future__ import generators
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
# (C) 2000, National Research Council of Canada
#
##############################################################################
#'$Id: HTMLgen.py,v 1.2 2007/01/04 18:11:58 quintijn Exp $'

# COPYRIGHT (C) 1996-9  ROBIN FRIEDRICH  email:Robin.Friedrich@pdq.net
# Permission to use, copy, modify, and distribute this software and
# its documentation for any purpose and without fee is hereby granted,
# provided that the above copyright notice appear in all copies and
# that both that copyright notice and this permission notice appear in
# supporting documentation.
# THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
# SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
# FITNESS, IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
# SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
# RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
# CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
# CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# This module is extensively revised by Quintijn Hoogenboom,
# QH softwaretraining & advies, 2003/2004/2005/2006.
# email:q.hoogenboom@antenna.nl
#
# this version is meant for the WhatCanISay functionality of voicecoder
#
__author__ = 'original: Robin Friedrich   friedrich@pythonpros.com, additions QH'
__version__ = '$Id: HTMLgen.py,v 1.2 2007/01/04 18:11:58 quintijn Exp $'


"""A class library for the generation of html documents


Started by Robin Friedrich, on many places adapted by QH software, training & advies

The commercial program "siteGen" (http://www.sitegen.nl) relies on HTMLgen as
underlying module.

Each HTML tag type has a supporting class which is responsible for
emitting itself as valid HTML formatted text. An attempt is made to
provide classes for newer HTML 3.2 and proposed tag elements.  The
definitive reference for HTML tag elements can be found at
[W3C].  Also, I used the HTML book by Musciano and
Kennedy from [O Reilly] (2nd. Ed.) as the guiding reference.

The Document classes are container objects which act as a focal point
to populate all the contents of a particular web page. It also can
enforce consistent document formating according to the guidelines from
the [Yale Web Style Manual].

Features include customization of document template graphics / colors
through use of resource files, minimizing the need for modifying or
subclassing from the module source code. Support for tables, frames,
forms (persistent and otherwise) and client-side imagemaps are included.
** stripped by QH.

A newer implementation for the Table support is now included,
TableLite().  In support of this there are new tag classes TD, TH, TR
and Caption.  These class instances can be assembled in any way to
populate the TableLite container object.


QH02112002: improved metatags, fixed problem with "prefix"
QH11022003: Href made in context of an abstract tag
QH10122003: stripping off trailing whitespace when giving the string of
            TD, TH or LI, because these caused unwanted markup
            
.. [W3C] http://www.W3.org/TR/REC-html32.html
.. [O Reilly] http://www.oreilly.com/catalog/html3/index.html
.. [Yale Web Style Manual] http://info.med.yale.edu/caim/manual/contents.html
"""
import string, re, time, os, types
import UserList, copy, sys

from utilsqh import path, isStringLike, intarray, curry
from htmlentitydefs import *
## for VCode exclude some things that are probably not needed:
##from PIL import Image, JpegImagePlugin, GifImagePlugin, ImageEnhance

class HTMLgenError(Exception): pass
DictType = type({})
IntType    = type(3)
ListType   = type([1])
TupleType  = type((1,2))
InstanceType = type(UserList.UserList())
def isString(d):
    return type(d) == types.StringType

CONTYPE = 'Content-Type: text/html '
#QH04112002
##DOCTYPE = '<html>\n'
DOCTYPE = """<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>"""
# 0 = minimal, 1 = normal, 2 = more
PRINTECHO = 0
#QH20102002 insert newline before each tag and before each end tag
# advise: ENDNL = ''
STARTNL = '\n'
ENDNL = ''

#################
# CLASS LIBRARY #
#################

#--------------------New stuff-revised QH January 2004---------------#
#--- basic classes are also used for document, because the append function
#--- is then identical (always do str before an append)
#
class AbstractTagSingle(dict):
    """Abstract base class for all tag markup classes not requiring
        a closure tag.

Only keyword arguments can be specified, unless the specialised
class receives them as positional arguments

In each class "tagname" and "attrs" must be specified.

Note: tagname and attrs MUST all be lower case.
All attrs are (when called in initialize) are converted to lowercase!    

If extra instance variables are used in a subclass, these must
be specified in "instance_variables". These remain case sensitive!

If attributes are required they can specified in
"required_attrs"

Attributes that start with "on" are supposed to be JavaScript
statements. The prefix "javascript:" and the trailing ";" may
not be specified in the call, they are put there in the
__setattr__ function

>>> HR()
{}
>>> str(HR())
'\\n<hr>'
>>> h = HR(Class="topmenu")
>>> str(h)
'\\n<hr class="topmenu">'
>>> str(HR())
'\\n<hr>'

If you want to change or add attributes later, only do this by
qualifying the parameter. If you use the direct dictionary entry,
it leads to unpredicted results.

>>> h.Class = "leftmenu"
>>> str(h).strip()
'<hr class="leftmenu">'

>>> h["Class"] = "picmenu"
>>> str(h)
'\\n<hrpicmenu class="leftmenu">'

Typos are caught:
>>> HR(klass="wrong")
Traceback (most recent call last):
HTMLgenError: invalid attribute "klass" in instance of <class 'HTMLgen.HR'>,  tagname "hr"

Calling an instance is not allowed, as is with AbstractTag:
>>> h()
Traceback (most recent call last):
TypeError: 'HR' object is not callable

Required attributes can be specified, as "href" in Base:
>>> str(Base(href="http://www.a.nl"))
'\\n<base href="http://www.a.nl">'
>>> str(Base())
Traceback (most recent call last):
HTMLgenError: required attribute(s) not given: ['href'], instance of <class 'HTMLgen.Base'> with tag name "base"

(Required) attributes can be skipped by setting them to None:    
>>> str(Base(href=None))
'\\n<base>'
>>> str(Base(href="http://www.a.nl", target=None))
'\\n<base href="http://www.a.nl">'

>>> str(TD(Href('abc.html', 'abcd'), onmouseover="status='goto abc';", onmouseout="status='';")).strip()
'<td onmouseout="status=\\'\\';" onmouseover="status=\\'goto abc\\';"><a href="abc.html">abcd</a></td>'

    """
    tagname = '' # to be provided by derived classes
    attrs = ()   # to be provided by derived classes
    instance_variables = ()      # as long as empty, no extra variables are allowed
    required_attrs = ()          # can ask for compulsory attributes
    strip_contents = 0           # 1 can put STARTNL and ENDNL extra
    def __init__(self, **kw):
        """only receive given keyword arguments,names are converted to lowercase

        """        
        dict.__init__(self)
        for name, value in kw.items():
            setattr(self, name, value)
    def __nonzero__(self):
        """always true!"""
        return True

    def __str__(self):
        """Generate an HTML formatted string for a single tag.

        All the attributes of the tag appended here, required_attrs first,
        then other non-event attributes, finally on-attributes
        """
        if self.strip_contents:
            start = ['<%s'%self.tagname]
        else:
            start = [STARTNL + '<%s'%self.tagname]
        mid = []
        end = []
        required = list(self.required_attrs)
        KEYS = self.keys()
        KEYS.sort()
        for k in KEYS:
            if self[k] == None:
                try:
                    required.remove(k)
                except ValueError:
                    pass
                continue
            if k in self.required_attrs:
                required.remove(k)
                # do these first:
                start.append(self[k])
            elif k[0:2] == 'on':
                # do javascript arguments last:
##                if k == 'onresize':
##                    print 'donotdo onresize'
##                    continue
##                else:
                end.append(self[k])
            else:
                mid.append(self[k])
        if self.strip_contents:
            end.append('>')
        else:
            end.append('>'+ENDNL)
        
        if required:
            raise HTMLgenError('required attribute(s) not given: %s, instance of %s with tag name "%s"'%
                               (required, self.__class__, self.tagname))

        return  ''.join(start)+''.join(mid)+''.join(end)


    def __add__(self, other):
        """returns a string of the contents of both added

        Here different combinations of AbstractTagSingle and AbstractTag
        are tested:
        >>> '<p>text</p>' + HR()
        '<p>text</p>\\n<hr>'
        >>> HR() + '<p>text</p>'
        '\\n<hr><p>text</p>'
        >>> HR() + HR()
        '\\n<hr>\\n<hr>'
        >>> HR() + Strong('hello') + HR() + Strong(' again')
        '\\n<hr><strong>hello</strong>\\n<hr><strong>again</strong>'
        >>> '<p>text</p>' + Strong('Hi there')
        '<p>text</p><strong>Hi there</strong>'
        >>> Strong('Hi there')+ '<p>text</p>'
        '<strong>Hi there</strong><p>text</p>'
        >>> '<p>text</p>' + HR() + Strong('hello') + HR() + Strong(' again') + '<p>end of text</p>'
        '<p>text</p>\\n<hr><strong>hello</strong>\\n<hr><strong>again</strong><p>end of text</p>'
        
        """
        if type(other) == types.StringType:
            return str(self) + other
        else:
            return str(self) + str(other)

    def __radd__(self, other):
        """returns a string of the contents of both added (first being non instance)

        """
        if type(other) == types.StringType:
            return other + str(self)
        else:
            return str(other) + str(self)

    def getAttrContents(self, name):
        """getting only the attr value (after the = sign)

>>> s = Header(1, "text", Class="abc")
>>> s.getAttrContents('class')
'abc'
>>> s.getAttrContents('style')
        """
        t = self.get(name)
        if name in self.attrs and t:
            t = t.split('="')[-1]
            if t:
                t = t[:-1]
        return t

    def appendToAttr(self, name, text, duplicates=0):
        """no duplicating of name and =

        behaves like a set,if text to add is already in the attribute,
        do not append it.
        
>>> s = Header(1, "text", Class="abc")
>>> s.getAttrContents('class')
'abc'
>>> s.appendToAttr('class', 'def')
>>> str(s).replace('\\n', '')
'<h1 class="abcdef">text</h1>'
>>> str(s).replace('\\n', '')
'<h1 class="abcdef">text</h1>'
>>> s.appendToAttr('style', 'hello')
>>> s.appendToAttr('style', 'hello')
>>> str(s).replace('\\n', '')
'<h1 class="abcdef" style="hello">text</h1>'
>>> s.appendToAttr('style', 'hello', duplicates=1)
>>> str(s).replace('\\n', '')
'<h1 class="abcdef" style="hellohello">text</h1>'
        """
        t = self.getAttrContents(name) or ''
        if t.find(text) == -1 or duplicates:
            t += text
        self.__setattr__(name, t)
    def prependToAttr(self, name, text, duplicates=0):
        """no duplicating of name and =

        behaves like a set,if text to add is already in the attribute,
        do not append it.
        
        """
        t = self.getAttrContents(name) or ''
        if t.find(text) == -1 or duplicates:
            t = text+t
        self.__setattr__(name, t)

    def __setattr__(self, name, value):
        """Intercept attribute assignments.
        -
        If the attribute is a legal HTML tag attribute add it
        to the instance dict, that is used for substitution in
        __str__,

        Otherwise set as instant variable (provided it is
        allowed through "instance_variables".
        """
        if name.lower() in self.attrs:
            name = name.lower()
            if value == None:
                dict.__setitem__(self, name, None)
            else:
                if type(value) == types.IntType and value < 0:
                    raise HTMLgenError('negative value |%s| of "%s" in instance of %s, tagname "%s"'%
                                       (value, name, self.__class__, self.tagname))
                    
                dict.__setitem__(self, name, ' %s="%s"' % (name, value))
        elif name in self.instance_variables:
            self.__dict__[name] = value
        else:
            raise HTMLgenError('invalid attribute "%s" in instance of %s,  tagname "%s"'%
                               (name, self.__class__, self.tagname))

    def __getattr__(self, name):
        """Intercept attribute retrieval.
        
        """
        name = name.lower()
        try:
            if name in self.attrs:
                return dict.__getitem__(self, name)
            else:
                return self.__dict__[name]
        except KeyError:
            if name in self.attrs or name in self.instance_variables:
                return ''
            elif name in self.required_attrs:
                raise HTMLgenError('tries to get required attribute, but this is not given: %s (%s, tag: %s)'%
                                   (name, self.__class__, self.tagname))
                
            else:
                raise HTMLgenError('KeyError in instance of %s, tag: %s, invalid variable asked for: %s'%
                               (self.__class__, self.tagname, name))
        
class AbstractTag(AbstractTagSingle):
    """Abstract base class for all tag markup classes requiring a closure tag.

    Keyword arguments can be specified, they are interpreted
    (almost always) as attributes for the tag, as in
    the ancestor class AbstractTagSingle.

    Other arguments (in front of the keyword arguments) can be
    given, they are inserted as a list of contents.
    
    >>> Strong()
    {}
    >>> str(Strong('this is very strong text'))
    '<strong>this is very strong text</strong>'
    >>> s = Strong('this is strong text', Strong('silly, but double strong'))
    >>> str(s)
    '<strong>this is strong text<strong>silly, but double strong</strong></strong>'

    Do not directly (afterwards) change attributes in the instance,
    this leads to unpredictable results:
    >>> s["Class"] = "leftmenu"
    >>> str(s)
    '<strongleftmenu>this is strong text<strong>silly, but double strong</strong></strong>'

    >>> s["klass"] = 3
    >>> str(s)
    Traceback (most recent call last):
    TypeError: sequence item 1: expected string, int found

    Typos are caught:
    >>> Strong(klass="wrong")
    Traceback (most recent call last):
    HTMLgenError: invalid attribute "klass" in instance of <class 'HTMLgen.Strong'>,  tagname "strong"

    Calling an instance allowed, see after at __call__

    Required attributes can be specified, as "href" in Base:

    (Required) attributes can be skipped by setting them to None:    

    The variable "strip_contents" can be switched on, in order to
    prevent leading or trailing white space in the contents of the
    tag. This is especially useful for "TD". Also extended to the whole
    content inclusive the tags.
    
    """
    instance_variables = ('contents',) # no extra instance variables allowed, unless overrided
    trailer = ''
    strip_contents = 0

    def __init__(self, *contents, **kw):
        self.contents = []
        for item in contents:
            self.contents.append(item)
        AbstractTagSingle.__init__(self, **kw)

    def __str__(self):
        """Generate an HTML formatted string for this object

        The start of the tag is taken from the ancestor class.
        
        After that the contents and after that the closing tag.

        If the contents are long, or if "trailer" is set to for
        example "\n", after the tag this trailer and/or an
        additional "\n" is put.

        If the flag "strip_contents" is set, whitespace is stripped
        off, this is especially useful for "TD".
        
        """
        start = AbstractTagSingle.__str__(self)
        if len(start) > 72 and not self.strip_contents:
                between = '\n'
        else:
                between = ''
            
        # get the contents:
        if self.strip_contents:
            c = join(self.contents).strip()
        else:
            c = join(self.contents)
            
        end = '</%s>%s' % (self.tagname, self.trailer)

        s = start + between + c + end
        if len(s) > 72 and not self.strip_contents:
            return s + '\n'
        else:
            return s

    def __call__(self, *contents):
        """Enable instances to be callable as text processing functions.
        With this call existing instances can be reused. Each time the contents
        are replaced by the contents in the call, and a string is returned
        directly.

        >>> S = Strong()
        >>> S('Hi!')
        '<strong>Hi!</strong>'
        >>> S('Hi!', ' there')
        '<strong>Hi! there</strong>'
        >>> S()
        '<strong></strong>'

        """
        self.empty()
        for c in contents:
            if type(c) == types.StringType:
                self.append(c)
            else:
                self.append(join(c))
        return str(self)

    def pop(self, index=None):
        """pop last item or item index points to

        """
##        print 'append %s items'% len(items)
        if index:
            return self.contents.pop(index)
        else:
            return self.contents.pop()
            

    def append(self, *items):
        """Append one or more items to the end of the container

        always make a string of the contents, so changes on an object
        are effectively fixed at each append statement.
        """
##        print 'append %s items'% len(items)
        for item in items:
            if type(item) == types.StringType:
##                print 'appending string %s chars: %s'% (len(item), item)
                self.contents.append(item)
            else:
                s = join(item)
##                print 'appending join %s chars: %s'% (len(s), s)
                self.contents.append(join(item))

    def prepend(self, *items):
        """Prepend one or more items to the top of the container.
        """
##        print 'prepend %s items'% len(items)
        for item in items:
            if type(item) == types.StringType:
##                print 'inserting %s chars: %s'% (len(item), item)
                self.contents.insert(0, item)
            else:
                s = join(item)
##                print 'inserting %s chars: %s'% (len(s), s)
                self.contents.insert(0, join(item))

    def empty(self):
        """Empty the contents of the container.
        """
        self.contents = []

    def __len__(self):
        """Return the integer length of the container list.
        """
        return len(self.contents)

    def last(self):
        """Return a reference to the last item in the container.
        """
        return self.contents[-1]

    def copy(self):
        """Return a full copy of CONTENTS ofthe object.
        """
        return copy.deepcopy(self.contents)

#======= NEW CLASS STRUCTURE ============
# HTMLgen 1       HTMLgen 2
# --------------- ------------------
# Document        SeriesDocument
# MinimalDocument SimpleDocument
#                 BasicDocument (base class)
#                 TemplateDocument
# QH only leaving BasicDocument and SimpleDocument,
# in sitegen.py SeriesQH is defined with very extensive elaborations.


class BasicDocument(AbstractTag):
    """Base class to define an HTML document.

    Subclass of AbstractTag, for the body part, so the bodypart behaves as
    an AbstractTag.

    Non-keyword arguments are taken as the initial contents for this object.

    Keyword arguments:
        title -- HTML TITLE attribute for document
        bgcolor -- background color expressed in hex-triplet or names from HTMLcolors.
        background -- background image filename
        cgi -- flag to indicate if this is used in CGI context (1 if it is)
        textcolor -- color to use for normal text
        linkcolor -- color to use for hyperlinks
        vlinkcolor -- color to use for visited hyperlinks
        alinkcolor -- color to use when hyperlink is active


    >>> b = BasicDocument()
    >>> b.append(Paragraph('hello world'))
    >>> b.title = "title of test BasicDocument"
    >>> str(b)
    '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\\n<html>\\n<head>\\n<title>title of test BasicDocument</title></head>\\n<body>\\n<p>hello world</p></body></html>'
    
    """
    tagname = 'body'
    attrs = ('bgcolor', 'background', 'textcolor', 'linkcolor',
             'vlinkcolor', 'alinkcolor', 'onresize', 'onload')

    def __init__(self, *contents, **kw):
        self.cgi = self.title = ''
        AbstractTag.__init__(self, *contents, **kw)

    def __str__(self):
        """return the basic document string, self being based on 'body' tag

        """        
        s = []
        if self.cgi:
            s.append('Content-Type: text/html ' + DOCTYPE)
        else:
            s.append(DOCTYPE)
##        s.append('\n<!-- This file is generated using Python HTMLgen module. -->\n')

        # build the HEAD and BODY tags
        s.append(self.html_head())
        # body tag is the AbstractTagSingle for this item:
        s.append(AbstractTag.__str__(self))
        # closing:              
        s.append('</html>')
        return join(s)

    def __setattr__(self, name, value):
        """Intercept attribute assignments.
        
        If the attribute is a legal HTML tag attribute add it
        to the instance dict, that is used for substitution in
        __str__,

        Otherwise set as instant variable. Drop the requirement that
        only variables that are contained in "instance_variables"
        maybe set from accessed.
        """
        if name.lower() in self.attrs:
            name = name.lower()
            if value == None:
                dict.__setitem__(self, name, None)
            else:
                dict.__setitem__(self, name, ' %s="%s"' % (name, value))
        else:
            self.__dict__[name] = value

    def __getattr__(self, name):
        """Intercept attribute retrieval.

        If a variable is nonexistent, raise a KeyError, like normal
        practice. This contrary to the AbstractTag mechanism.
        """
        if name.lower() in self.attrs:
            name = name.lower()
            try:
                return dict.__getitem__(self, name)
            except KeyError:
                return ''
        else:
            return self.__dict__[name]

    def __nonzero__(self):
        """always give true for a document
        """
        return 1

    def html_head(self):
        """Generate the HEAD, TITLE and BODY tags.
        """
        return '\n<head>\n<title>%s</title></head>' % self.title

    def __call__(self, *contents):
        """switch off direct call for the document instances

        >>> b = BasicDocument()
        >>> str(b('abc'))
        Traceback (most recent call last):
        HTMLgenError: calling is not allowed for this instance of <class 'HTMLgen.BasicDocument'> (contents: ('abc',))

        """
        raise HTMLgenError("calling is not allowed for this instance of %s (contents: %s)"% \
                           (self.__class__, contents))
    

    def append_file(self, filename, marker_function = None):
        """Add the contents of a file to the document.

        filename -- the filename of the file to be read [string]
        marker_function -- a callable object which the text read from
          the file will be passed through before being added to the
          document.
        """
        f = open(mpath(filename), 'r')
        if marker_function:
            self.append(marker_function(f.read()))
        else:
            self.append(f.read())
        f.close()

    def write(self, filename = None):
        """Emit the Document HTML to a file or standard output.

        Will not overwrite file is it exists and is textually the same.
        In Unix you can use environment variables in filenames.
        Will print to stdout if no argument given.
        """
        s = str(self)
        if filename:
            return writestring2fileifdifferent(s, filename)
        else:
            import sys
            sys.stdout.write(s)


class SimpleDocument(BasicDocument):
    """Supports all features of a self contained document.

    This includes support for CSS1, meta and base tags, and embedded
    scripts.

    First constructor argument is resource file containing document
    attribute settings.

    The __str__ method is larger: inside the <body> now come
    header(), contents (called throuhg body()) and footer() 

    Basic output:    

>>> s = SimpleDocument()
>>> str(s)
Traceback (most recent call last):
HTMLgenError: instance of <class 'HTMLgen.SimpleDocument'> has no contents!
>>> s = SimpleDocument()
>>> s.append(Paragraph('hello world'))        
>>> str(s)
'<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">\\n<html><head>\\n<title></title></head>\\n<body>\\n<p>&nbsp;</p>\\n<p>hello world</p>\\n<p>&nbsp;</p>\\n</body></html>'

With title and some other additions: ('\\n' stripped off)

>>> s.title = 'Title'
>>> s.append(Pre('abc'))
>>> str(s).replace('\\n', '')[75:-7]
'<title>Title</title></head><body><p>&nbsp;</p><p>hello world</p><pre>abc</pre><p>&nbsp;</p></body>'

More calls of one tag:

>>> s.append(Pre('def'))
>>> s.prepend(Pre('012'))
>>> pre = Pre('hij')
>>> s.append(pre)
>>> s.append(pre)

And calling as a function:

>>> s.append(pre('uvw'))
>>> s.append(pre('xyz'))
>>> str(s).replace('\\n', '')[75:-7]
'<title>Title</title></head><body><p>&nbsp;</p><pre>012</pre><p>hello world</p><pre>abc</pre><pre>def</pre><pre>hij</pre><pre>hij</pre><pre>uvw</pre><pre>xyz</pre><p>&nbsp;</p></body>'




    """
    base = None
    style = None
    stylesheet = None
    meta = {}
    script = None
    prefix = ""
    keywords = ''
    headlink = None
    
    def __init__(self, **kw):
        self.contents = []
        self.onloadList = []
        self.onresizeList = []
        BasicDocument.__init__(self, **kw)


    def __str__(self):
        """return the SimpleDocument string,

        start tag based on 'body' tag of AbstractTagSingle,
        rest joined together between header and footer
        """        
        s = []
        if self.cgi:
            s.append('Content-Type: text/html ' + DOCTYPE)
        else:
            s.append(DOCTYPE)
##        s.append('\n<!-- This file is generated using Python HTMLgen module -->\n')        

        # body tag is the AbstractTagSingle for this item:
        s.append(self.header())
        footer = self.footer() # could be intercepted in body!
        s.append(self.body())
        s.append(footer)
        # if header, footer or body changed things in attributes
        # they are used now:
        # get the javascript attributes
        
        for t in self.onloadList:
             self.appendToAttr('onload', t)
            
        if self.onresizeList:
            self.prependToAttr('onload', "afterLoad();")
            if self.doOnloadAtOnresize:
                self.appendToAttr('onresize', "onResizeReload();")
            else:
                self.appendToAttr('onresize', "onResize();")
        s.insert(1, AbstractTagSingle.__str__(self))
        # build the HEAD and BODY tags
        s.insert(1, self.html_head())
                 
        s.append('\n</body>')
        # closing, do the direct to execute javascript things (if onResizeList is true)
        ss = join(self.doJavascriptFunctions())
        if ss:
            s.append(ss)
        s.append('</html>')
        return join(s)

    def doJavascriptFunctions(self):
        """overload in HtmlGen"""
        pass

    def html_head(self):
        """Generate the HEAD TITLE and BODY tags.
        """
        s = []
        if len(self.title) > 60:
            T = map(string.strip, self.title.split(','))
            if len(T) == 1:
                t = T[0][-60:]
            else:
                T = T[1:]
                t = ', '.join(T)
                while len(t) > 60 and len(T) > 1:
                    T = T[1:]
                    t = ', '.join(T)
            s.append('\n<head>\n<title>%s</title>' %  t)
        else:
            s.append('\n<head>\n<title>%s</title>' %  self.title)
            
        #QH02112002
        if 'description' in dir(self):
            s.append(Meta(name="description", content = self.description))
        if self.keywords:
            K = ', '.join(self.keywords)
##            if len(K) > 87:
##                T = self.keywords[1:]
##                t = ', '.join(T)
##                while len(t) > 600 and len(T) > 1:
##                    T = T[1:]
##                    t = ', '.join(T)
##                s.append(Meta(name='keywords', content = t))
##            else:
            s.append(Meta(name='keywords', content = K))
        if self.meta:
            if type(self.meta) == DictType:
                for k,v in self.meta.items():
                    if v:
                        s.append(Meta(equiv=k, content=v))
            else:
                self.append(self.meta)
        if self.base: s.append(str(self.base))
        if self.stylesheet:
        ### QH
            if not hasattr(self, "prefix"):
                print "self, no prefix", self.__name__
                self.prefix = path("")
            if type(self.prefix) == types.StringType:
                self.prefix = path(self.prefix)
            urlunix = self.prefix/self.stylesheet

            s.append('\n<link rel="stylesheet" href="%s" type="text/css" title="%s">' \
                     % (urlunix, self.stylesheet))
        if self.style:
            s.append('\n<style>%s</style>' % self.style)
            
        if self.script: # for javascripts
            if type(self.script) in (TupleType, ListType):
                for script in self.script:
                    s.append(str(script))
            else:
                s.append(str(self.script))
        # for prev next or rel="icon":
        if self.headlink:
            h=[]
            if type(self.headlink) in (TupleType, ListType):
                for headlink in self.headlink:
                    h.append(str(headlink))
                s.append(''.join(h).strip())
            else:
                raise HTMLgenError('self.headlink must be a list or a tuple, not: %s'% self.headlink)
            
        s.append('</head>\n')
        S = join(s).replace('\n\n', '\n')
        return S.strip()

    def body(self):
        """head in the contents of the page

        basically this will be the contents of the instance,
        but also menus and other things can be inserted here

        """
        if self.contents:
            return '\n'.join(self.contents)
        raise HTMLgenError('instance of %s has no contents!'% self.__class__)

    def header(self):
        return str(Paragraph('&nbsp;'))
    def footer(self):
        return str(Paragraph('&nbsp;'))

#===================

class Meta:
    """Set document Meta-information.

    The META element is used within the HEAD element to embed
    document meta-information not defined by other HTML elements.

    Keywords supported

        name  -- NAME element attribute (default: 'keywords')
        equiv  -- will map to the HTTP-EQUIV attribute
        content -- mandatory attribute (default: 'python,HTMLgen')
        url -- URL naturally

    Example:

        Meta( name='keywords', content='eggs,spam,beans' )
    """
    def __init__(self, **kw):
        ##QH:
        self.equiv = ''
        self.name  = ''
        self.content = ''
        self.url = ''
        for item in kw.keys():
            self.__dict__[item] = kw[item]

    def __str__(self):
        s = [STARTNL+'<meta']
        if self.name and not self.content:
            return ''
        if self.equiv in ['author']:
            self.name = self.equiv
            self.equiv = None
        if self.equiv: s.append(' http-equiv="%s"' % self.equiv)
        if self.name:  s.append(' name="%s"' % self.name)
        if self.content: s.append(' content="%s"' % self.content)
        if self.name and not self.content:
            return ''
        if self.url: s.append(' url="%s"' % self.url)
        s.append('>'+ENDNL)
        return string.join(s, '')

##### Client-side Imagemap Support #####

class Map:
    """Used to name and describe a client-side image map.

    The *areas* argument is a list of Area objects.
    Keyword arg is supported for *name*, which defines the map name
    to be used with the usemap attribute of an Image class instance.
    """
    def __init__(self, areas = None, **kw):
        self.areas = areas or []
        self.name = ''
        for item in kw.keys():
            self.__dict__[item] = kw[item]

    def __str__(self):
        s = [STARTNL+'<map name="%s">\n' % self.name]
        for area in self.areas:
            s.append(str(area))
        s.append('</map>\n')
        return string.join(s, '')


class MailTo:
    """A Mailto href

    First argument is an email address, optional second argument is
    the text shown as the underlined hyperlink. Default is the email
    address. Optional third argument is a Subject: for the email.
    """
    def __init__(self, address='', text=None, subject=None):
        self.address = address
        self.text = text or self.antispam(address)
        self.subject = subject

    def __str__(self):
        adr = self.antispam(self.address)
        if self.subject:
            adr = adr + "?subject=" + string.strip(self.subject)
        return (STARTNL+'<a href="mailto:%s">%s</a>') % (adr, self.text)

    def antispam(self, address):
        """Process a string with HTML encodings to defeat address spiders.
        """
##        from whrandom import choice
        buffer = map(None, address)
##        for i in range(0, len(address), choice((2,3,4))):
        # one coded two normal:
        for i in range(0, len(address), 3):
            buffer[i] = '&#%d;' % ord(buffer[i])
        return string.join(buffer,'')

MAILTO = Mailto = MailTo # aliases

class P:
    """Just echo a <p> tag."""
    def __str__(self):
        return STARTNL+'<p>\n'

# List constructs

class List(UserList.UserList):
    """Will generate a bulleted list given a list argument.

    Now supports rendering a list into multiple columns by setting the
    *columns* attribute to a number greater than one. This is
    implemented using tables and you can also set a background color
    for the list itself by using the *bgcolor* attribute.

    Supports nested lists, i.e. lists of lists. Each time a list is
    encountered in a list it will indent those contents w.r.t. the
    prior list entry. This can continue indefinitely through nested
    lists although there are only three different bullets provided by
    the browser (typically).

    Optional keyword *indent* can be used to indicate whether you want
    the list to start left justified or indented. *indent=0* will make
    it left justified. The default is to indent.

    Optional keyword *type* can be set to either disk, circle, or
    square to specify what kind of symbol is used for each list item's
    bullet. (Netscape extension)

    Since we inherit from the UserList class any normal list
    operations work on instances of this class.  Any list contents
    will do. Each of the items will be emitted in html if they are
    themselves objects from this module.
    Aliases: UL, BulletList
    >>> str(List()).strip()
    '<ul></ul>'
    
    >>> str(List([])).strip()
    '<ul></ul>'
    >>> str(List(['a'])).strip()
    '<ul><li>a</li></ul>'
    >>> 
    

    
    """

    I_am_a_list = 1
    tagname = 'ul'
    attrs = ('type','align','class','id','style')
    flags = ('compact',)
    columns = 1
    bgcolor = ''
    pad = '    '
    indent = 1
    def __init__(self, list = None, **kw):
        self.data = []
        self.lvl = 0
        if list:
            if type(list) == type(self.data):
                self.data[:] = list
            else:
                self.data[:] = list.data[:]
        for item in kw.keys():
            self.__dict__[string.lower(item)] = kw[item]

    def __getslice__(self, i, j):
        newlist = copy.copy(self)
        newlist.data = self.data[i:j]
        newlist.columns = 1 # don't forget that the copy will carry
                            # the old attribute value. set to 1
        return newlist

    def multi_column_table(self):
        """Return a table containing the list sliced into columns.
        """
        slices = self.column_slices(self.columns)
        table = TableLite(border=0, cellpadding=3)
        if self.bgcolor: table.bgcolor = self.bgcolor
        for begin, end in slices:
            column = TD(self[begin:end], valign='top', html_escape='no')
            table.append(column)
        return table

    def column_slices(self, columns=1):
        """Calculate a list of index pairs bounding column slices.
        """
        list_len = len(self.data)
        col_len, remainder = divmod(list_len, columns)
        if remainder: col_len = col_len + 1
        indexpairs = []
        if columns > 1:
            for column in range(columns):
                col_end = (column+1)*col_len
                if col_end < list_len:
                    indexpairs.append((column*col_len, col_end))
                else:
                    indexpairs.append((column*col_len, list_len))
        else:
            indexpairs.append((0, list_len))
        return indexpairs

    def __str__(self):
        if self.columns > 1: # go to the new multicolumn feature
            return str(self.multi_column_table())
        # same as before
        self.firstitem = 1
        self.s = []
        if self.indent:
            self.s.append(self.pad*self.lvl + self.start_element())
        for item in self.data: #start processing main list
            itemtype = type(item)
            if itemtype == InstanceType:
                try: # in case it's a nested list object
                    if item.I_am_a_list:
                        itemtype = ListType
                except AttributeError:
                    pass
            if itemtype == ListType: #process the sub list
                self.sub_list(item)
            else:
                self.s.append(self.render_list_item(item))

        if self.indent: #close out this level of list
            self.s.append(self.pad*self.lvl + self.end_element())
        self.lvl = 0
        return ''.join(self.s)

    def sub_list(self, list):
        """Recursive method for generating a subordinate list
        """
        self.lvl = self.lvl + 1
        if type(list) == InstanceType:
            try:
                if list.I_am_a_list: #render the List object
                    list.lvl = self.lvl
                    self.s.append(str(list))
            except AttributeError:
                pass
        else:
            self.s.append(self.pad*self.lvl + self.start_element())
            for item in list:
                itemtype = type(item)
                if itemtype == InstanceType:
                    try: #could be another nested List child object
                        if item.I_am_a_list:
                            itemtype = ListType
                    except AttributeError:
                        pass
                if itemtype == ListType:
                    self.sub_list(item) #recurse for sub lists
                else: # or just render it
                    self.s.append(self.render_list_item(item))
            # close out this list level
            self.s.append(self.pad*self.lvl + self.end_element())
        self.lvl = self.lvl - 1 #decrement indentation level

    def render_list_item(self, item):
        """Renders the individual list items

        Overloaded by child classes to represent other list styles.
        """
        return '%s<li>%s</li>' % (self.pad*self.lvl, item)

    def start_element(self):
        """Generic creator for the HTML element opening tag.

        Reads tagname, attrs and flags to return appropriate tag.
        """
        s = ['<' + self.tagname]
        for attr in self.attrs:
            try:
                s.append(' %s="%s"' % (attr, getattr(self, attr)))
            except AttributeError:
                pass
        for flag in self.flags:
            try:
                x = getattr(self, flag)
                s.append(' %s' % flag)
            except AttributeError:
                pass
        s.append('>')
        return ''.join(s)

    def end_element(self):
        """Closes the HTML element
        """
        return '</%s>' % self.tagname

    def append(self, *items):
        """Append entries to the end of the list
        """
        for item in items:
            self.data.append(item)

UL = BulletList = List  #Aliases

class OrderedList(List):
    """Will generate a numbered list given a list arg.

    Optional keyword *type* can be used to specify whether you want
    the list items marked with: capital letters (type='A'), small
    letters (type='a'), large Roman numerals (type='I'), small Roman
    numerals (type='i'). The default is arabic numbers. The other
    types are HTML3.2 only and may not be supported by browsers yet.
    Any list contents will do. Each of the items will be emitted
    in HTML if they are themselves objects.
    """
    tagname = 'ol'
    attrs = ('type','class','id','style')

OL = NumberedList = OrderedList

class DefinitionList(List):
    """Show a series of items and item definitions.

    Arg is a list of tuple pairs:
    [(string/object,string/object),(,)...]  1st item in each pair is
    the word to be defined. It will be rendered in bold. 2nd is the
    string which will be indented to it's next-line-right. If the
    *compact* flag is set to non-empty, the definition side will be
    placed on the same line.  Example

>>> t("DefinitionList([( 4 , 'Number after 3') , ( 1 , 'Unity')] )")
<dl><dt><b>4</b></dt><dd>Number after 3</dd><dt><b>1</b></dt><dd>Unity</dd></dl>

    """
    tagname = 'dl'
    attrs = ('class','id','style')
    flags = ('compact',)
    def render_list_item(self, item):
        """Overload method to perform DT/DD markup.
        """
        return '%s<dt><b>%s</b></dt><dd>%s</dd>' % (self.pad*self.lvl, item[0], item[1])

DL = DefinitionList

class DefinitionListIndexed(List):
    """Show a series of items and item definitions. used by dld abbreviations

    Arg is a list of tuple triples:
    [(string/object,idstring, string/object),(,)...]  1st item in each pair is
    the word to be defined. It will be rendered in bold.
    2nd is the index that is inserted at that place

    3nd is the
    string which will be indented to it's next-line-right. If the
    *compact* flag is set to non-empty, the definition side will be
    placed on the same line.  Example

>>> t("DefinitionListIndexed([( 4 , 'four', 'Number after 3') , ( 1 , 'one', 'Unity')] )")
<dl><dt id="four"><b>4</b></dt><dd>Number after 3</dd>
<dt id="one"><b>1</b></dt><dd>Unity</dd>
</dl>

"""
    tagname = 'dl'
    attrs = ('class','id','style')
    flags = ('compact',)
    def render_list_item(self, item):
        """Overload method to perform DT/DD markup.
        """
        return '%s<dt id="%s"><b>%s</b></dt><dd>%s</dd>\n' % (self.pad*self.lvl, item[1], item[0], item[2])



class ImageBulletList(List):
    """Show a list of images with adjoining text(or object).

    Arg is a list of tuple pairs: [(Image_obj, string/object),(,)...]
    
    >>> T('ImageBulletList([(Img("pijl.gif"), "voorbeeld van ImageBulletList"),'
    ...                     '(Img("v.gif"), "tweede regel met ander image")])')
    <dl><dt>
    <img src="pijl.gif"></dt><dd>voorbeeld van ImageBulletList</dd>
    <dt>
    <img src="v.gif"></dt><dd>tweede regel met ander image</dd>
    </dl>


    """
    tagname = 'dl'
    attrs = ()
    flags = ()
    def render_list_item(self, item):
        """Overload method to take first item from an item tuple and
        setting it next to the second item, using BR to separate list items.
        """
        return '<dt>%s</dt><dd>%s</dd>\n' % (str(item[0]), str(item[1]))

class NonBulletList(List):
    """Generate a raw indented list without bullet symbols.

    Arg is a list of python objects:
    """
    tagname = 'ul'
    attrs = ()
    flags = ()
    def render_list_item(self, item):
        """Overload method to take first item from an item tuple and
        setting it next to the second item, using BR to separate list items.
        """
        return '%s%s<br>\n' % (self.pad*self.lvl, item)

# extra lists for sitegen:

class LeftMenuList(List):
    """Generate a possibly indented list, with classed list items

    leftoffAlternate through parent if given as self.

    If leftoffAlternate leftmenuJavascript is advised to put on!    
    
    if leftoffAlternate = 1, classes are leftoff0 and leftoff1

    leftmenuIndent default 20 from parent.    

    firstLevelSeparate: classes at first level append with "first" (vs)

>>> t("LeftMenuList( [('lefton','','left on', 0, 1), ('leftoff','aaa.html','AAA'), ('leftoff', 'bbb.html', 'BBB')])")
<table class="leftmenu" border="0" cellpadding="0" cellspacing="0" width="100%">
<tr><td class="lefton">left on</td></tr>
<tr><td class="leftoff"><a href="aaa.html">AAA</a></td></tr>
<tr><td class="leftoff"><a href="bbb.html">BBB</a></td></tr></table>

>>> t("LeftMenuList( [('lefton','','left on'), [('leftoff','aaa.html','AAA'), ('leftoff', 'bbb.html', 'BBB')]])")
<table class="leftmenu" border="0" cellpadding="0" cellspacing="0" width="100%">
<tr><td class="lefton" colspan="2">left on</td></tr>    
<tr><td class="back" width="20">&nbsp;</td><td class="leftoff"><a href="aaa.html">AAA</a></td></tr>
<tr><td class="back" width="20">&nbsp;</td><td class="leftoff"><a href="bbb.html">BBB</a></td></tr>
    </table>

    Testing of leftoffAlternate skipping, because parent cannot be set here.

    
    """
    tagname = 'table'
    attrs = ()
    flags = ()

    # leftToggle is only for leftoffAlternate,
    # gotoLine is only if leftmenuJavascript is on.
    instance_variables = ('leftoffAlternate', 'leftToggle', 'parent',
                          'gotoLine', 'leftmenuJavascript', 'insertId', 'firstLevelSeparate')

    def __init__(self, lijst=None, **kw):
        self.parent = None
            
        List.__init__(self, lijst, **kw)
        if self.parent:
            self.insertId = self.parent.leftmenuInsertId
            self.leftmenuIndent = self.parent.leftmenuIndent
            self.leftoffAlternate = self.parent.leftoffAlternate
            self.leftmenuJavascript = self.parent.leftmenuJavascript
            if self.leftmenuJavascript:
                self.gotoLine = self.parent.navbuttonTexts[self.parent.language]['goto']
            self.firstLevelSeparate = self.parent.firstLevelSeparate
                
        else:
            self.insertId = None
            self.leftmenuIndent = 20
            self.leftoffAlternate = None
            self.firstLevelSeparate = None
            self.leftmenuJavascript = 0

        self.nestingLevel = howDeepNested(self.data) - 1
        
    def render_list_item(self, item):
        """Overload method to take first item from an item tuple and
        setting it next to the second item, using BR to separate list items.
        """
        s = []
        if not (type(item) == types.TupleType):
            raise HTMLgenError('in render_list_item from "%s", invalid type of item: "%s"'% (self.__class__.__name__, item))
##        print 'lvl, maxlevel, item:', self.lvl, self.nestingLevel, item[1]
        if len(item) > 3:
            cl, linkTo, linkText = item[:3]
        else:
            cl, linkTo, linkText = item
        if self.firstLevelSeparate and not self.lvl:
            cl += "first"
        if cl == 'leftoff' and self.leftoffAlternate and self.nestingLevel == self.lvl:
            cl = 'leftoff%s'% self.leftToggle
            self.leftToggle = (self.leftToggle+1)%2
        else:
            self.leftToggle = 0 # after lefton start again with 0
        i = 0
        while i < self.lvl:
            s.append(TD("&nbsp;", Class="back", width=self.leftmenuIndent))
            i = i + 1        
        if self.nestingLevel>self.lvl:
            colspan=self.nestingLevel-self.lvl+1
        else:
            colspan=None
        if self.leftmenuJavascript and linkTo:
            gotoText = self.gotoLine % linkText
            s.append(TD(Href(linkTo, linkText), Class=cl,colspan=colspan,                 
                        onclick="location='%s';"% linkTo, 
                        onmouseover="status='%s';"% escapeJSString(gotoText),
                        onmouseout="status='';"))
        elif linkTo:            
            s.append(TD(Href(linkTo, linkText), Class=cl, colspan=colspan))
        else:
            s.append(TD(linkText, Class=cl, colspan=colspan))
        id = None
        if self.insertId:
            id = getIdFromFrom(linkTo) or None
        t = [TR(join(s), id=id)]
        
        return join(t)

    def start_element(self):
        self.leftToggle = 0
        if self.lvl:
            return ''
        else:
            s = ['<table class="leftmenu" border="0" cellpadding="0" cellspacing="0" width="100%">']
##            s.append(self.horizontalSpacer())
            return join(s)

    def end_element(self):
        if self.lvl:
            return ''
        return('</table>\n')

class SitemapList(LeftMenuList):
    """Generate a possibly indented list, with classed list items

    In this case all the items are tuples of length 3:
    (Class, linkTo, niceName)

    if linkTo false: niceName = text
    if linkTo true: niceName = page instance

    If no link (and no id necessary), fill in empty strings as in first example
    
    """
    tagname = 'table'
    attrs = ()
    flags = ()

    def __init__(self, lijst=None, **kw):
        LeftMenuList.__init__(self, lijst, **kw)
        
    def render_list_item(self, item):
        """Overload method to take first item from an item tuple and
        setting it next to the second item, using BR to separate list items.
        """
        s = []
        if not (type(item) == types.TupleType and len(item) == 3):
            raise HTMLgenError('in render_list_item from "%s", invalid type: %s or length: %s (%s)'%
                               (self.__class__.__name__, type(item), len(item), `item`))
##        print 'lvl, maxlevel, item:', self.lvl, self.nestingLevel, item[1]
        cl, linkTo, pageTo = item
        if linkTo:
            try:
                linkText = pageTo.niceName
                if linkText == 'index':
                    linkText = pageTo.parent.niceName
                ref = Href(linkTo, linkText, id=pageTo.getId())
            except AttributeError:
                linkText  = '???'
                ref = linkText
##                if linkText.endswith('/index.html'):
##                    linkText = linkText.replace('/index.html', '')
        else:
            linkText = pageTo
            ref = linkText
        i = 0
        while i < self.lvl:
            s.append(TD(Class="back", width=20))
            i = i + 1
        if self.nestingLevel>self.lvl:
            s.append(TD(ref, Class=cl, colspan=self.nestingLevel-self.lvl+1))
        else:
            s.append(TD(ref, Class=cl))
        t = [TR(join(s))]
##        t.append(self.horizontalSpacer())
        
        return join(t)

    def start_element(self):
        if self.lvl:
            return ''
        else:
            s = ['<table class="sitemap" border="0" cellpadding="0" cellspacing="0" width="100%">']
##            s.append(self.horizontalSpacer())
            return join(s)


class ClassedList(List):
    def render_list_item(self, item):
        """Renders the individual list items

        Overloaded by child classes to represent other list styles.
        """
        return '%s<li class="%s">%s\n' % (self.pad*self.lvl, item[0], join(item[1]).strip())



def howDeepNested(list):
    if type(list) == type([]):
        nl = map(howDeepNested, list)
        if nl == []:
            return 1
        else:
            return 1 + max(nl)
    else:
        return 0

def getInnerList(list):
    for l in list:
        if isList(l):
            return getInnerList(l)
    else:
        return list


####### FORM TAGS ########
class Form:
    """Define a user filled form. Uses POST method.

    *cgi* is the URL to the CGI processing program.  Input objects
    (any others as well) are appended to this container widget.

    Keywords

        name -- name of the form
        submit -- The Input object to be used as the submit button.
                  If none specified a Submit button will automatically
                  be appended to the form. Do not manually append your
                  submit button. HTMLgen will append it for you.
        reset  -- Input object to be used as a reset button.
        target -- set a TARGET attribute
        enctype -- specify an Encoding type.
        onSubmit -- script, which is executed, when the form is submitted
    """
    def __init__(self, cgi = None, **kw):
        self.contents = []
        self.cgi = cgi
        self.submit = Input(type='submit', name='SubmitButton', value='Send')
        self.reset = None
        self.target = None
        self.enctype = None
        self.name = None
        self.onSubmit = ''
        overlay_values(self, kw)

    def append(self, *items):
        """Append any number of items to the form container.
        """
        for item in items:
            self.contents.append(str(item))

    def __str__(self):
        s = ['\n<form method="POST"']
        if self.cgi: s.append(' action="%s"' % self.cgi)
        if self.enctype: s.append(' enctype="%s"' % self.enctype)
        if self.target: s.append(' target="%s"' % self.target)
        if self.name: s.append(' name="%s"' % self.name)
        if self.onSubmit: s.append(' onSubmit="%s"' % self.onSubmit)
        s.append('>\n')
        s = s + self.contents
        s.append(str(self.submit))
        if self.reset: s.append(str(self.reset))
        s.append('\n</form>\n')
        return string.join(s, '')


def overlay_values(obj, dict):
    """Adds each item from dict to the given object iff there already
    exists such a key. Raises KeyError if you try to update the value
    of non-existing keys.
    """
    for key in dict.keys():
        if hasattr(obj, key):
            obj.__dict__[key] = dict[key]
        else:
            raise KeyError(`key` + ' not a keyword for ' + obj.__class__.__name__)


class Input:
    """General Form Input tags.

    Keyword Arguments

        type -- 'TEXT' (default) Supported types include password, checkbox,
                      radio, file, submit, reset, hidden.
        name -- provides the datum name
        value -- the initial value of the input item
        checked --  flag indicating if the item is checked initially
        size -- size of the widget (e.g. size=10 for a text widget is it's width)
        maxlength -- maximum number of characters accepted by the textfield.
        border -- border width in pixels for an image type.
        align -- top|middle|bottom align w.r.t. adjoining text for image types.
        llabel  --  an optional string set to the left of the widget
        rlabel  --  an optional string set to the right of the widget
        onBlur -- script, which is executed, when the field loses focus,
                  useful for the text-type
        onChange -- script, which is executed, when the field value changed,
                    useful for the text-type
        onClick -- script, which is executed, when the field in clicked,
                   useful for the button, checkbox, radio, submit, reset type
        onFocus -- script, which is executed, when the field receives focus,
                   useful for the text-type
        onSelect -- script, which is executed, when part of the field
                    is selected, useful for the text-type
    """
    re_type = re.compile('text|password|checkbox|radio|image|button|file|submit|reset|hidden',
                            re.IGNORECASE)
    def __init__(self, **kw):
        self.type = 'TEXT'
        self.name = 'Default_Name'
        self.value = None
        self.checked = ''
        self.size = 0
        self.maxlength = 0
        self.llabel = ''
        self.rlabel = ''
        self.onBlur = ''
        self.onChange = ''
        self.onClick = ''
        self.onFocus = ''
        self.onSelect = ''
        self.border = None
        self.align = ''
        for item in kw.keys():
            if self.__dict__.has_key(item):
                self.__dict__[item] = kw[item]
            else:
                raise KeyError, `item`+' not a valid parameter of the Input class.'
        if Input.re_type.search(self.type) is None:
            raise KeyError, `self.type`+' not a valid type of Input class.'

    def __str__(self):
        s = []
        if self.llabel: s.append(str(self.llabel))
        s.append('\n<input')
        if self.type: s.append(' type="%s"' % self.type)
        if self.name: s.append(' name="%s"' % self.name)
        if self.value is not None: s.append(' value="%s"' % self.value)
        if self.checked: s.append(' checked')
        if self.size: s.append(' size=%s' % self.size)
        if self.maxlength: s.append(' maxlength=%s' % self.maxlength)
        if self.onBlur: s.append(' onBlur="%s"' % self.onBlur)
        if self.onChange: s.append(' onChange="%s"' % self.onChange)
        if self.onClick: s.append(' onClick="%s"' % self.onClick)
        if self.onFocus: s.append(' onFocus="%s"' % self.onFocus)
        if self.onSelect: s.append(' onSelect="%s"' % self.onSelect)
        if self.border is not None: s.append(' border="%s"' % self.border)
        if self.align: s.append(' align="%s"' % self.align)
        s.append('>')
        if self.rlabel: s.append(str(self.rlabel))
        return string.join(s, '')


class Select(UserList.UserList):
    """Used to define a list widget or option widget.

    Pass a list of strings to show a list with those values. Alternatively
    can pass a list of tuple pairs. Each pair contains the displayed string
    and it's associatated value mapping. If no value mapping is needed just
    use something that evaluates to None.

    Keyword Arguments:

        name -- provides the datum name
        size -- the visual size. 1 means use an option popup widget.
                               >=2 means use a list widget with that many lines.
        multiple -- flag to indicate whether multiple selections are supported.
        selected -- list of values to be shown as pre-selected.
        onBlur -- script, which is executed, when the field loses focus
        onChange -- script, which is executed, when the field value changed
        onFocus -- script, which is executed, when the field receives focus
    """
    def __init__(self, data=None, **kw):
        UserList.UserList.__init__(self, data)
        self.name = ''
        self.size = 1
        self.multiple = None
        self.selected = []
        self.onBlur = ''
        self.onChange = ''
        self.onFocus = ''
        for item in kw.keys():
            if self.__dict__.has_key(item):
                self.__dict__[item] = kw[item]
            else:
                raise KeyError, `item`+' not a valid parameter of the Select class.'

    def __str__(self):
        s = ['<select name="%s"' % self.name]
        if self.size: s.append(' size=%s' % self.size)
        if self.multiple: s.append(' multiple')
        if self.onBlur: s.append(' onBlur="%s"' % self.onBlur)
        if self.onChange: s.append(' onChange="%s"' % self.onChange)
        if self.onFocus: s.append(' onFocus="%s"' % self.onFocus)
        s.append('>\n')
        if type(self.data[0]) is TupleType:
            for item, value in self.data:
                s.append('<option')
                if value is not None:
                    s.append(' Value="%s"' % value)
                if value in self.selected:
                    s.append(' selected')
                else:
                    if item in self.selected:
                        s.append(' selected')
                s.append('>%s\n' % item)
        else:
            for item in self.data:
                if item not in self.selected:
                    s.append('<option>%s\n' % item)
                else:
                    s.append('<option selected>%s\n' % item)
        s.append('</select>\n')
        return string.join(s, '')

class Textarea:
    """Used for an entry widget to type multi-line text (for forms).

    Keyword Arguments:

        rows -- sets the number of text rows. (default=4)
        cols -- sets the number of text columns. (default=40)
        onBlur -- script, which is executed, when the field loses focus
        onChange -- script, which is executed, when the field value changed
        onFocus -- script, which is executed, when the field receives focus
        onSelect -- script, which is executed, when part of the field
                    is selected
    """
    def __init__(self, text='', **kw):
        self.text = text
        self.name = 'text_area'
        self.rows = 4
        self.cols = 40
        self.onBlur = ''
        self.onChange = ''
        self.onFocus = ''
        self.onSelect = ''
        for item in kw.keys():
            if self.__dict__.has_key(item):
                self.__dict__[item] = kw[item]
            else:
                raise KeyError, `item`+' not a valid parameter of the Textarea class.'

    def __str__(self):
        s = ['<textarea name="%s" rows=%s cols=%s' % (self.name, self.rows, self.cols)]
        if self.onBlur: s.append(' onBlur="%s"' % self.onBlur)
        if self.onChange: s.append(' onChange="%s"' % self.onChange)
        if self.onFocus: s.append(' onFocus="%s"' % self.onFocus)
        if self.onSelect: s.append(' onSelect="%s"' % self.onSelect)
        s.append('>')
        s.append(str(self.text))
        s.append('</textarea>')
        return string.join(s, '')

class Script(AbstractTag):
    """Construct a Script

    Keyword Arguments

        Defaults in (parenthesis).  Keyword parameters may be set as attributes of
        the instantiated script object as well.

        language -- specifies the language (default 'javascript')
        src -- specifies the location
        type -- (default 'text/javascript')
        code -- script code, which is printed in comments, to hide it from non
                java-script browsers

>>> str(Script(src='../../test.js')).strip()
'<script language="javascript" src="../../test.js" type="text/javascript"></script>'
>>> str(Script('alert("this is javascript");', 'alert("hello");')).strip()
'<script language="javascript" type="text/javascript"><!--\\nalert("this is javascript");alert("hello");\\n// --></script>'
    """
    tagname = 'script'
    attrs = ('src', 'type', 'language')
    strip_contents = 1

    def __init__(self, *arg, **kw):
        # do the standard initialise:        

        AbstractTag.__init__(self, *arg, **kw)
        self.setdefault('type', ' type="text/javascript"')        
        self.setdefault('language', ' language="javascript"')        

    def __str__(self):
        if self.contents:
            c = '<!--\n%s\n// -->'% join(self.contents)
            self.contents = [c]
            return AbstractTag.__str__(self)
        else:
            return AbstractTag.__str__(self).replace("\n", "")
####################

class Noscript(AbstractTag):
    """Construct the noscript tag
    >>> c = [Paragraph("hello"), Paragraph("there")]
    >>> str(Noscript(c)).replace("\\n", "")
    '<noscript><p>hello</p><p>there</p></noscript>'

    """
    tagname = 'noscript'
    attrs = ()
    strip_contents = 1

class Object(AbstractTag):
    """Construct a object tag

    """
    tagname = 'object'
    attrs = ('width', 'height')


class Param(AbstractTagSingle):
    """Construct a param tag

    """
    tagname = 'param'
    attrs = ('name', 'value')

class Embed(AbstractTag):
    """Embed an application in this document.
    """
    tagname = 'embed'
    attrs = ('align', 'border', 'height', 'hidden', 'hspace',
             'name', 'palette', 'pluginspage', 'src', 'type',
             'units', 'vspace', 'width')


class HeadLink(AbstractTagSingle):
    """links for the head tag. Make a list of these """
    tagname = 'link'
    attrs = ('rel', 'href', 'type', 'rev')
    
    
####################
    

class TABLE(AbstractTag):
    """Construct a Table with Python lists, revised QH

    Instantiate with only keyword arguments as defined below
    Set object.heading to a list of strings representing the column headings.
    Set object.body to a list of lists representing rows.
    
    Keyword Parameters

        first is dummy for historic reasons.

        Defaults in (parenthesis).  Keyword parameters may be set as attributes of the
        instantiated table object as well.

        captionalign -- 'top'|'bottom'  specifies the location of the table title ('top')
        columnwidths -- list of (relative) widths of the respective columns
        headingcolspan -- a list specifying the number of columns spanned by that heading
               index. e.g. t.colspan = [2,2] will place 2 headings spanning
               2 columns each (assuming the body has 4 columns).
        heading --  list of strings, the length of which determine the number of
                   columns.  ( ['&nbsp;']*3 )
        body -- a list of lists in row major order containing strings or objects
               to populate the body of the table. ( [['&nbsp;']*3] )
        cellalign --    'left'|'right'|'center'  text alignment for the columns
                        a string like above, or a list of strings:
                        if the list is shorter than the number of columns the last item
                        is taken
        celllinebreaks -- 2|1|0 group flag to determine if a ";" (semicolon),
                                newline char in body text will be
                                converted to <br> symbols;
                                2 : ";" breaks lines
                                1 : newline chars breaks lines (default)
                                0 :  this is not done 
        table attributes (some can also be supplied in the stylesheet at TABLE.table):
        border -- the width in pixels of the bevel effect around the table (2)
        cellpadding -- the distance between cell text and the cell boundary (4)
        cellspacing -- the width of the cell borders themselves (1)
        width -- the width of the entire table with the current window width ('100%')
        title -- title for the whole table, default No title

        class names are added for
            - whole table (default: "table"),
            - heading row ("th")
            - even and odd rows ("td0" and "td1")
            
>>> t("TABLE(body=[['aap', 'noot'],[2, 3], [4, 5]])")
<table border="1" cellpadding="4" cellspacing="0" class="table" width="100%">
<tr><td class="td0">aap</td><td class="td0">noot</td></tr>
<tr><td class="td1">2</td><td class="td1">3</td></tr>
<tr><td class="td0">4</td><td class="td0">5</td></tr></table>

>>> t("TABLE('', body=[['aap', 'noot'],[2, 3], [4, 5]])")
<table border="1" cellpadding="4" cellspacing="0" class="table" width="100%">
<tr><td class="td0">aap</td><td class="td0">noot</td></tr>
<tr><td class="td1">2</td><td class="td1">3</td></tr>
<tr><td class="td0">4</td><td class="td0">5</td></tr></table>

>>> t("TABLE(heading=['kop', 'staart'], body=[['aap', 'noot']], width='50%')")
<table border="1" cellpadding="4" cellspacing="0" class="table" width="50%">
<tr><th class="th">kop</th><th class="th">staart</th></tr>
<tr><td class="td0">aap</td><td class="td0">noot</td></tr></table>

>>> str(TABLE(header="abc"))
Traceback (most recent call last):
File "C:\Python22\lib\doctest.py", line 430, in _run_examples_inner
HTMLgenError: invalid attribute "header" in instance of <class 'HTMLgen.TABLE'>,  tagname "table"
    """
    tagname = 'table'
    attrs = ('class','id', 'style', 'align', 'background', 'border',
             'bordercolor', 'bordercolordark', 'bordercolorlight',
             'cols', 'frame', 'cellpadding', 'cellspacing',
             'height', 'hspace', 'width', 'bgcolor', 'nowrap',
             'rules', 'valign', 'vspace', 'title')
    instance_variables = ('contents', 'title', 'heading', 'body', 'celllinebreaks',
                          'cellalign','captionalign', 'headingcolspan',
                          'columnwidths')      # as long as empty, no extra variables are allowed
    required_attrs = ()          # can ask for compulsory attributes
    strip_contents = 0
    
    def __init__(self, dummy=None, **kw):
        """Arg1 is a string title for the table caption, optional keyword
        arguments follow.
        """
        # Specify the default values
        for v in self.__class__.instance_variables:
            setattr(self, v, None)
        self.captionalign = 'top'
        self.border = 1
        self.cellpadding = 4
        self.cellspacing = 0
        self.width = '100%'
        self.celllinebreaks = 1
        setattr(self, "class", "table")
        # Now overlay the keyword arguments from caller
        AbstractTag.__init__(self, **kw)

    def __str__(self):
        """Generates the html for the entire table.
        """
        # Construct heading spec
        #  can handle multi-row headings. colspan is a list specifying how many
        #  columns the i-th element should span.
        tr = TR()
        self.empty()
        if self.heading:
            if type(self.heading) != types.ListType:
                raise HTMLgenError('TABLE tag, heading must be a list of texts, not: "%s"'% `self.heading`)
            colspans = self.headingcolspan or [None]*len(self.heading)
            if type(colspans) != types.ListType:
                raise HTMLgenError('TABLE tag, headingcolspan must be a list: "%s"'% `colspans`)
            if len(colspans) != len(self.heading):
                raise HTMLgenError('TABLE tag, length of heading (%s) must the same as length of headingcolspan (%s),\nheading: %s\nheadingcolspan: %s'% \
                                   (len(self.heading), len(colspans), `self.heading`, `colspans`))
            for h, c in zip(self.heading, colspans):
                tr.append(TH(h, colspan=c, Class="th"))
            self.append(tr)
            tr.empty()

        if not self.body or type(self.body) != types.ListType:
             raise HTMLgenError('TABLE tag, no body or invalid body: "%s"'% `self.body`)


        classNumber = 1
        for i in range(len(self.body)):
            tr.empty()
            classNumber += 1
            className = 'td%s'%(classNumber%2) # first data row = td0
            for j in range(len(self.body[i])):
                t = str(self.body[i][j]).strip()
                if self.celllinebreaks == 1:
                    t = '<br>'.join(map(string.strip, t.split('\n')))
                elif self.celllinebreaks == 2:
                    t = '<br>'.join(map(string.strip, t.split(';')))
                tr.append(TD(t, Class=className))
            self.append(tr)
        return AbstractTag.__str__(self)

Table = TABLE # alias

class TABLEUSP(TABLE):
    """create onmouseover event for displaying extra text in the central cell

    each element has short (USP) and long (explanation) text packed in a tuple.

    a. only one column of entry's (for nimba)
    b. two columns of entry's (put in header) (for sg)

    threeimages are expected, imagebase, imageactive and imagedone, pointing
    to a gif, but leave off .gif!. Prefix can be added if not on homepage.
    
    1. is the start button, for example a red arrow pointing to the right
    2. is when active
    3. for example Green v, if mouseover has occurred

    must go with JavaScript functions tdshow and tdhide, which work with id's       
       
    can also pass parent=self, and runPythonCode is executed for each TD text,
    and prefix is added.

example no heading, 2 columns 2 rows, with leftheight (nimba):

>>> T('TABLEUSP(heading = [], border=0, leftheight=200, body='
...          '[("stichting een", "omschrijving van een."),'
...          '("stichting twee", "omschrijving van twee")])')                           
<table border="0" cellpadding="4" cellspacing="0" class="table" width="100%">
<tr><td class="tdusps" height="200" id="tdusp0t" onmouseout="javascript:tdhide('tdusp0');" onmouseover="javascript:tdshow('tdusp0');"><ul><li>stichting een</li></ul></td><td class="tduspe" id="tduspe" rowspan="4" width="65%" onmouseover="javascript:tdshowextra();">&nbsp;</td></tr>
<tr><td class="tdusph" id="tdusp0h">omschrijving van een.</td></tr>
<tr><td class="tdusps" height="200" id="tdusp1t" onmouseout="javascript:tdhide('tdusp1');" onmouseover="javascript:tdshow('tdusp1');"><ul><li>stichting twee</li></ul></td></tr>
<tr><td class="tdusph" id="tdusp1h">omschrijving van twee</td></tr></table>


example 3 columns, sitegen, sg:

>>> t('TABLEUSP(heading = ["eenvoudig:", "doeltreffend:"], border=0, body='
...          '[("mooi", "omschrijving van zeer mooi.|Twee regels.")])')                           
<table border="0" cellpadding="4" cellspacing="0" class="table" width="100%">
<tr><th class="th" width="33%">eenvoudig:</th><th class="th" id="the" width="34%">&nbsp;</th><th class="th" width="34%">doeltreffend:</th></tr>
<tr><td class="tdusps" id="tdusp0t" onmouseout="javascript:tdhide('tdusp0');" onmouseover="javascript:tdshow('tdusp0');"><ul><li>mooi</li></ul></td><td class="tduspe" id="tduspe" rowspan="2" width="34%" onmouseover="javascript:tdshowextra();">&nbsp;</td></tr>
<tr><td class="tdusph" id="tdusp0h">omschrijving van zeer mooi.|Twee regels.</td></tr>
</table>



"""
    instance_variables = ('parent', 'contents', 'title', 'heading', 'body', 'celllinebreaks',
                          'cellalign','captionalign', 'headingcolspan', 'leftheight',
                          'columnwidths', 'idstart', 
                          'starttext', 'widths', 'ncols')
                          # as long as empty, no extra variables are allowed

    def __init__(self, **kw):
        """
        """
        self.parent = None
        self.starttext = ''
        setattr(self, "class", "tableusp")

        TABLE.__init__(self, **kw)
        self.idstart = 'tdusp'
        self.widths = ['35%', '65%']
        self.ncols = 2

    def __str__(self):
        """Generates the html for the entire TABLEUSP
        
        """
        # Construct heading spec
        #  can handle multi-row headings. colspan is a list specifying how many
        #  columns the i-th element should span.
        tr = TR()
        tr2 = TR()
        idnum = 0
        self.empty()
        if self.heading:
            if type(self.heading) != types.ListType:
                raise HTMLgenError('TABLE tag, heading must be a list of texts, not: "%s"'% `self.heading`)

            if len(self.heading) == 2:
                self.ncols = 3  # example sg
                self.widths = ['33%', '34%', '33%']
            elif len(self.heading) > 2: 
                raise HTMLgenError('TABLE tag, length of heading must be a list maximum 2n not: "%s"'% `self.heading`)
               
            colnum = 0
            for h in self.heading:
                tr.append(TH(h, Class="th", width=self.widths[colnum]))
                if not colnum:
                    # central cell for displaying explanation results
                    colnum += 1
                    tr.append(TH("&nbsp;", Class="th", id="the", width=self.widths[colnum]))
            self.append(tr)
            tr.empty()
            
        if not self.body or type(self.body) != types.ListType:
             raise HTMLgenError('TABLE tag, no body or invalid body: "%s"'% `self.body`)

        #input is list of tuples,which consist of the usp text and the longer text.
        #the longer text should come in a rowbelow the visible text.
        nRows = len(self.body)
        classHidden = self.idStart+'h'
        classShow = self.idStart+'s'
        for i in range(nRows):
            idName = '%s%s'% (self.idstart, i)
            t, extra = self.body[i]
            t = t.strip()
            prefix = ""
            if self.parent:
                prefix = self.parent.prefix
                t = self.parent.runPythonCode(t)
                extra = self.parent.runPythonCode(extra)
            tr2.append(TD(extra, id=idName+"h", Class=classHidden))

            inp = List([t])
            # the short USP texts:
            tdinp = TD(inp, Class=classShow,
                         id=idName+'t', height=self.leftheight,
                         onmouseover="javascript:tdshow('%s');"% idName,
                             onmouseout="javascript:tdhide('%s');"% idName)
            tr.append(tdinp)

            if i  == 0:
                # the spanning right column:
                text = self.starttext or "&nbsp;"
                tr.append(TD(text, 
                             rowspan = nRows*2, width=self.widths[1],
                             onmouseover="javascript:tdshowextra();",
                             Class=self.idstart+'e', id=self.idstart+'e'))

            if self.ncols == 2 or i%2:
                self.append(tr)
                tr.empty()        
                self.append(tr2)
                tr2.empty()

        if tr.contents:       # possibly the pending line:
            self.append(tr)
            self.append(tr2)

        return AbstractTag.__str__(self)


#QH09022003
#in AbstractTag gezet:

class Base(AbstractTagSingle):
    """Specify the base URL for all relative URLs in this document.

    >>> b = Base()
    >>> str(b)
    Traceback (most recent call last):
    HTMLgenError: required attribute(s) not given: ['href'], instance of <class 'HTMLgen.Base'> with tag name "base"
    >>> str(Base(href="../im/a.html"))
    '\\n<base href="../im/a.html">'

    """
    required_attrs = ('href',)
    tagname = 'base'
    attrs = ('href', 'target')

class BR(AbstractTagSingle):
    """Break tag. Argument is an integer integer multiplier. BR(2)=='<br><br>'
    """
    tagname = 'br'
    attrs = ('clear',)
    
    def __str__(self):
        s = AbstractTagSingle.__str__(self)
        if self.args and type(self.args[0]) is IntType:
            return s*self.args[0]
        else:
            return s



class Img(AbstractTagSingle):
    """Inlined Image

    The *filename* (src) argument is a filename, or URL of a graphic image,
    or a triple of ( filename, width, height ) where dimensions are in
    pixels.
    
    Keyword Arguments

        prefix -- Relative path or URL to directory containing the image
        and all attributes for the tag (path instance or string!)

    normal use, first argument can be the src, but maybe omitted and
    filled in as keyword parameter:
    
>>> str(Img('a.jpg'))
'\\n<img src="a.jpg">'
>>> i1 = Img(src='b.jpg')
>>> str(i1)
'\\n<img src="b.jpg">'
>>> str(Img(('c.jpg', 10, 20)))
'\\n<img src="c.jpg" height="20" width="10">'

prefix can be either string or path instance:

>>> str(Img('b.jpg', prefix='../'))
'\\n<img src="../b.jpg">'
>>> str(Img('b.jpg', prefix=path('../')))
'\\n<img src="../b.jpg">'

and testing the possible errors for this tag:

>>> str(Img('b', 'c'))
Traceback (most recent call last):
TypeError: __init__() takes at most 2 arguments (3 given)
>>> str(Img(Class="test"))
Traceback (most recent call last):
HTMLgenError: required attribute(s) not given: ['src'], instance of <class 'HTMLgen.Img'> with tag name "img"
>>> str(Img())
Traceback (most recent call last):
HTMLgenError: required attribute(s) not given: ['src'], instance of <class 'HTMLgen.Img'> with tag name "img"
>>> str(Img('b.jpg', src='c.jpg'))
Traceback (most recent call last):
HTMLgenError: Img tag src enterd twice: "'b.jpg'" and "c.jpg"
>>> str(Img(('b.jpg', 10, 30), src='c.jpg'))
Traceback (most recent call last):
HTMLgenError: Img tag src enterd twice: "('b.jpg', 10, 30)" and "c.jpg"

Alt behaviour: if no alt is given, the filename will be used as alt-text.
Instead in many cases you will like to give alt="" as parameters.
>>> str(Img('b.jpg'))
'\\n<img src="b.jpg">'
>>> str(Img('b.jpg', alt=''))
'\\n<img src="b.jpg" alt="">'

The attributes starting with "on" should be given at the end of the argument list:    
>>> str(Img('b.jpg', onmouseover="status='blablabla'", onmouseout="status=''", width=23, height=45))
'\\n<img src="b.jpg" height="45" width="23" onmouseout="status=\\'\\'" onmouseover="status=\\'blablabla\\'">'
>>> str(Img(('b.jpg', 23, 45), onmouseover="status='blablabla'", onmouseout="status=''", width=23, height=45))
'\\n<img src="b.jpg" height="45" width="23" onmouseout="status=\\'\\'" onmouseover="status=\\'blablabla\\'">'

>>> p = Picture('abc.jpg', width = 10, height = 20)
>>> print str(Img(p)).strip()
<img src="abc.jpg" alt="abc" height="20" width="10">
>>> print str(Img(p, alt = 'hello, this is a picture image')).strip()
<img src="abc.jpg" alt="hello, this is a picture image" height="20" width="10">


    """
    tagname = 'img'
    attrs = ('src', 'height', 'width', 'alt', 'border', 'align', 'class','id',
             'hspace','vspace', 'lowsrc', 'name', 'style', 'usemap', 'ismap',
             'onmouseover', 'onmouseout')
    required_attrs = ('src',)
    
    def __init__(self, image=None, **kw):
        """image is normally given as first argument.

        """        
        # intercept special variable prefix:
        prefix = path('')
        if 'prefix' in kw:
            prefix = path(kw['prefix'])
            del kw['prefix']
        if type(prefix) == types.StringType:
            prefix = path(self.prefix)
            
        if image:
           if 'src' in kw:
               raise HTMLgenError('Img tag src enterd twice: "%s" and "%s"'% (`image`, kw['src']))
           if type(image) == types.TupleType and len(image) == 3:
               src = image[0]
               self.width = image[1]
               self.height = image[2]
            
           elif isStringLike(image):
               src =  image
           elif isinstance(image, Picture):
                for k in ('height', 'width'):
                    setattr(self, k, getattr(self, k) or getattr(image, k))
                src = image.name
                if 'alt' not in dir(self):
                    self.alt = image.alt or src[:-4]
           else:
               raise HTMLgenError('Img tag src not string or tuple of length 3 or Picture instance: %s'% `image`)
           self.src = prefix/src
                
        # do the standard initialise:        
        AbstractTagSingle.__init__(self, **kw)

IMG = Img # alias

class BR(AbstractTagSingle):
    """Break tag. Argument is an integer integer multiplier. BR(2)=='<br><br>'
    """
    tagname = 'br'
    attrs = ('clear',)

    def __str__(self):
        s = AbstractTagSingle.__str__(self)
        if self.args and type(self.args[0]) is IntType:
            return s*self.args[0]
        else:
            return s

class BaseFont(AbstractTagSingle):
    """Specify the font size for subsequent text.
    """
    tagname = 'basefont'
    attrs = ('color', 'name', 'size')


class HR(AbstractTagSingle):
    """Break the current text flow and insert a horizontal rule.

    >>> str(HR())
    '\\n<hr>'
    >>> str(HR(Class="a"))
    '\\n<hr class="a">'
    >>> str(HR())
    '\\n<hr>'

    """
    tagname = 'hr'
    attrs = ('align', 'class','id', 'color', 'noshade', 'size',
             'style', 'width')
    


class Href(AbstractTag):
    """Generate a hyperlink.

    Argument 1 is the URL and argument 2 is the hyperlink text.
    >>> str(Href('http://www.buitenwesten.nl/index.html', 'Surf to BuitenWesten', target="blank"))
    '\\n<a href="http://www.buitenwesten.nl/index.html" target="blank">Surf to BuitenWesten</a>\\n'

    >>> str(Href())
    Traceback (most recent call last):
    HTMLgenError: tag "Href" keyword "href" is missing {}
    >>> str(Href('http://a.nl/index.html', 'url doubled', href="http://b.nl/index.html"))
    Traceback (most recent call last):
    HTMLgenError: tag "Href", url given twice, in url: "http://a.nl/index.html" and in href: "http://b.nl/index.html"

    text omitted:    
    >>> str(Href('http://a.nl/index.html')).strip()
    '<a href="http://a.nl/index.html">a.nl/index.html</a>'

    """
    required_attrs = ('href',)
    tagname = 'a'
    attrs = ('href', 'name', 'accesskey', 'charset', 'class','coords', 'dir','hreflang',
             'id','lang', 'onblur', 'onclick', 'ondblclick', 'onfocus',
             'onkeydown', 'onkeypress', 'onkeyup', 'onmousedown', 'onmousemove',
             'onmouseout', 'onmouseover', 'onmouseup', 'rel', 'rev', 'shape', 
             'style', 'style', 'tabindex', 'target', 'title', 'type')

    def __init__(self, url='', text='', **kw):
        if url:
            if 'href' in kw:
                raise HTMLgenError('tag "Href", url given twice, in url: "%s" and in href: "%s"'% (url, kw['href']))
            kw['href'] = url
        if 'href' not in kw:
            raise HTMLgenError('tag "Href" keyword "href" is missing %s'% (kw))
            
          
        if not text:            
            text = kw['href']
            if isStringLike(text):
                if text.startswith('http://'):
                    text = text[7:]
            else:
                text = ''
        AbstractTag.__init__(self, join(text), **kw)
        

A = HREF = Href # alias

class Name(Href):
    """Generate a named anchor.

    Arg *url* is a string or URL object,
    Arg *text* is optional string or object to be highlighted as the anchor.
    >>> str(Name('#top', 'goto top'))
    '\\n<a href="#top">goto top</a>'


    """

NAME = Name # alias




class Area(AbstractTagSingle):
    """Specify a click-sensitive area of an image.

    The area is linked to a HREF specified by the *href* attribute.
    The *coords* attribute is required and describes the position of
    an area (in pixels) of the image in comma-separated x,y
    coordinates where the upper-left corner is "0,0". For shape='rect'
    (the default), it is "left,top,right,bottom". For shape='circle',
    it is "center_x,center_y,radius". For shape='polygon', it is
    successive x,y vertices of the polygon. If the first and last
    coordinates are not the same, then a segment is inferred to close
    the polygon. If no *href* keyword is given a *NOHREF* will be
    generated indicating that this region should generate no links.

    Keyword Arguments

        href --  Typically a reference to an image
        coords --  string holding a list of coordinates defining
        shape  -- 'rect'|'circle'|'polygon'
    """
    tagname = 'area'
    attrs = ('alt','class','coords','href','id','name',
         'onmouseout','onmouseover','shape','target')
    def __init__(self, **kw):
        """setting 'href' to special value

        """
        if 'href' not in kw:
            kw['href'] = ' nohref'
            AbstractTagSingle.__init__(self, **kw)
            del kw['href']
        else:
            AbstractTagSingle.__init__(self, **kw)
            

###### FRAME SUPPORT ######

class Frameset(AbstractTag):
    """Define a Frameset to contain Frames or more Framesets"""
    tagname = 'frameset'
    attrs = ('border','bordercolor','cols','frameborder','framespacing','onblur',
         'onfocus','onload','onunload','rows')

class NoFrames(AbstractTag):
    """Issue a message on browsers that don't support frames"""
    tagname = 'noframes'
    attrs = ()

    def __init__(self, *contents, **kw):
        AbstractTag.__init__(self)
        for content in contents: self.append(content)
        for name, value in kw.items(): self.__setattr__(name,value)
        if len(contents) == 0:
            self.append(Heading(2,'Frame ALERT!',align='center'),
                Para("""This document is designed to be viewed using Netscape's
                Frame features.  If you are seeing this message, you are using
                a frame challenged browser."""),
                Para('A ',Strong('Frame-capable'),' browser can be retrieved from',
                     Href('http://home.netscape.com/','Netscape Communications'),
                     ' or ',
                     Href('http://www.microsoft.com/','Microsoft')))

class Frame(AbstractTag):
    """Define the characteristics of an individual frame.

    Keywords Arguments

        src  -- is a HREF which points to the initial contents of the frame.
        name -- is the window name used by others to direct content into this frame.
        marginwidth -- is the number of pixels used to pad the left and right
               sides of the frame.
        marginheight -- is the number of pixels used to pad the top and bottom
               sides of the frame.
        scrolling -- is used to indicate scrolling policy set to 'yes'|'no'|'auto'
        noresize -- is a flag which instructs the browser to disallow frame resizing.
               set to non zero lock size ( noresize=1 ).
    """

    tagname = 'frame'
    attrs = ('align','bordercolor','frameborder','marginheight','marginwidth','name',
         'noresize','scrolling','src')



class Paragraph(AbstractTag):
    """Define a Paragraph.

    Takes a single string/object argument and the optional
    keyword argument 'align' which may be one of (left, right,
    center).  As always, Class and style keywords are supported.
    **Not to be confused with class P**. That is
    just for inserting a para break.

    Example:

    >>> str(Paragraph('Some text to center', align='center'))
    '\\n<p align="center">Some text to center</p>'
    """
    tagname = 'p'
    attrs = ('class','id', 'style', 'align')

Para = Paragraph # Alias

# Headings

class Heading(AbstractTag):
    """Heading markups for H1 - H6

    Heading(level, text, **kw)

    The *level* arg is an integer for the level of the heading.
    Valid levels are 1-6.
    The *text* arg is a string (or any object) for the text of the heading.
    Keyword arguments are align, Class, and style.

    For example:
    >>> h = Heading(2, 'Chapter 3', align='center')
    >>> str(h)
    '\\n<h2 align="center">Chapter 3</h2>'
    >>> h('Chapter 4')
    '\\n<h2 align="center">Chapter 4</h2>'
    >>> str(Header('number for gotten'))
    Traceback (most recent call last):
    HTMLgenError: headerNumber not valid for Header tag: number for gotten
    
    """
    attrs = ('class','id', 'style', 'align')
    instance_variables = ('contents', 'tagname')

    def __init__(self, headerNumber, *args, **kw):
        """number for header as first parameter

        """
        if headerNumber not in (0, 1, 2, 3, 4, 5, 6):
            raise HTMLgenError('headerNumber not valid for Header tag: %s'% headerNumber)
        self.tagname = 'h%s'%headerNumber
        AbstractTag.__init__(self, *args, **kw)


H = Head = Header = Heading # Aliases

class Caption(AbstractTag):
    """Define a caption for a table.
    """
    tagname = 'caption'
    attrs = ('class','id', 'style', 'align', 'valign')

class TH(AbstractTag):
    """Define a table header cell.
    """
    tagname = 'th'
    attrs = ('class','id', 'style', 'nowrap', 'align','valign','rowspan',
             'colspan', 'height', 'width', 'bgcolor', 'background',
             'bordercolor', 'bordercolordark', 'bordercolorlight',
             'onmouseover', 'onmouseout',
            )
    trailer = ''
    strip_contents = 1


# provide debug possibilities for TD
#QH05122003
TDDEBUG = 0

genCols = None

WIDTHS =  None
HEIGHTS = None
rowNum = None
colNum = None


def colorGraying(startColor):
    """give next gray scale of three r, g, b letters, if color = "rrggbb"

    generaror!!!!    
    if color in other format, return same color
    
    """
    fixColors = ["red", "blue", "green", "yellow", "lime", "aqua",
                 "maroon", "olive", "silver"]
    hexVals = ["F", "C", "9", "6"]
    lenh = len(hexVals)
    fixIndex = 0
    primaryColors = {"red": ("F", "0", "0"),
                     "blue": ("0", "0", "F"),
                     "green": ("0", "F", "0"),
                     "magenta": ("F", "0", "F"),
                     "yellow": ("F", "F", "0"),
                     "cyan": ("0", "F", "F")
                     }
    doFixColors = not startColor
    if startColor:
        if len(startColor) == 7 and startColor[0] == "#":
            r, g, b = startColor[1].upper(), startColor[3].upper(), startColor[5].upper()
        elif len(startColor) == 4 and startColor[0] == "#":
            r, g, b = startColor[1].upper(), startColor[2].upper(), startColor[3].upper()
        elif startColor in primaryColors:
            r, g, b = primaryColors[startColor]
        elif startColor in fixColors:
            fixIndex = fixColors.index(startColor)
            doFixColors = 1
        else:
            r = g = b = 0

    if doFixColors:
        while 1:
            yield fixColors[fixIndex]
            fixIndex += 1
            if fixIndex >= len(fixColors):
                fixIndex = 0

    elif r == 0 and g == 0 and b == 0:
        while 1:
            yield startColor

    else:
        while 1:
            yield "#" + r + r + g + g + b + b    
            r = getNext(r, hexVals, lenh)
            g = getNext(g, hexVals, lenh)
            b = getNext(b, hexVals, lenh)

def getNext(c, hexVals, lenh):    
    if c == "0":
        return c
    elif c in hexVals:
        ic = hexVals.index(c)
        if ic == lenh-1:
            return hexVals[0]
        else:
            return hexVals[ic+1]
    else:
        return c

def setTDDebug(value, startColor=None, giveTable=1):
    """set TD Debug value to 0 (off), 1 or 2, optional with color

    setting off also resets the TDDebugColor.

    value = 0: switching debug mode off
    value = 1: giving cells bright background colors, fixed or variable
               according to the second variable
    value = 2: like 1, but remove all contents from the cell, and also remove a
               "Class"-attribute if present

    value = -1: hold on temporarily (for a inner table)

    giveTable gives width and heigth table in HTML format to be inserted in the test page.
    simply do self.append(setTDDebug(0)). At switching off time.

    When this variable is set, this color is taken for filling the background.
    If "startColor" is not given, the TDColors are cycled through, giving a
    colorfull table!

    When you want to mark for example different rows with red and green,
    for the first row you call 'setTDDebug(1, "red")', and for the 
    second row:setTDDebug(1, "green")'. Switching off simply is a call to
    setTDDebug(0).

    If you give as color one of the primary colours, or "#FF0000" or "#0F0" etc
    these colours are treated as greying out colors.

    If "startColor" it is one of the list of fixColors (like "aqua" or "silver"),
    this color is started with.
    
>>> t('FullTable(TR(TD("test tddebug uit", width="300")))')
<table border="0" cellpadding="0" cellspacing="0" width="100%">
<tr><td width="300">test tddebug uit</td></tr></table>

>>> setTDDebug(1, "red")
>>> t('FullTable(TR(TD("test tddebug aan, rood 1", width="351")))')
<table border="0" cellpadding="0" cellspacing="0" width="100%">
<tr><td bgcolor="#FF0000" width="351" onmouseout="status=''" onmouseover="status=' width=351'">test tddebug aan, rood 1</td></tr>
</table>

>>> t('setTDDebug(0)')
<table border="1" cellpadding="4" cellspacing="0" class="table" width="100%">
<tr><th class="th">Widths</th><th class="th">Heights</th></tr>
<tr><td class="td0"><table border="1" cellpadding="4" cellspacing="0" class="table" width="100%">
<tr><td class="td0"></td><td class="td0">0</td><td class="td0">total</td></tr>
<tr><td class="td1">0:</td><td class="td1">351</td><td class="td1">351</td></tr>
</table></td><td class="td0"><table border="1" cellpadding="4" cellspacing="0" class="table" width="100%">
<tr><td class="td0"></td><td class="td0">0</td></tr>
<tr><td class="td1">0:</td><td class="td1">0</td></tr>
<tr><td class="td0"></td><td class="td0">---</td></tr>
<tr><td class="td1"></td><td class="td1">0</td></tr></table></td></tr>
</table>

>>> t('FullTable(TR(TD("test tddebug off again", width="300")))')
<table border="0" cellpadding="0" cellspacing="0" width="100%">
<tr><td width="300">test tddebug off again</td></tr></table>


<table border="0" cellpadding="0" cellspacing="0">
<tr>
<td width="300">test tddebug off again</td></tr></table>

>>> setTDDebug(0, giveTable=0)
no debugging for tables is switched on, no switching off is required

>>> setTDDebug(2)
>>> t('FullTable (TR(TD("test tddebug op 2", colspan=2, height=28, width="300")),'
...              'TR(TD("second row", Class="testing", height=45),'
...                 'TD("second cell", width=100)))')
<table border="0" cellpadding="0" cellspacing="0" width="100%">
<tr><td bgcolor="red" class="tiny" colspan="2" height="28" width="300" onmouseout="status=''" onmouseover="status=' width=300 height=28 colspan=2'">&nbsp;</td></tr>
<tr><td bgcolor="blue" class="tiny" height="45" onmouseout="status=''" onmouseover="status=' height=45'">&nbsp;</td><td bgcolor="green" class="tiny" width="100" onmouseout="status=''" onmouseover="status=' width=100'">&nbsp;</td></tr>
</table>


returning to 0, but no debug table:

>>> t('setTDDebug(0, giveTable=0)')
None


other way or building up table:
>>> setTDDebug(1)
>>> tl = FullTable()
>>> tr = TR()
>>> tr.append(TD('one', width=50, height="89"), TD('two', width="75%"),TD('three', width=40))
>>> tl.append(tr)
>>> tr.empty()
>>> setTDDebug(0, giveTable=0)

>>> setTDDebug(1)
>>> tl = FullTable()
>>> tr = TR()
>>> tr.append(TD('spanning 2 x 2', width=50, height="89", rowspan=2, colspan=2), TD('three', width="75%", height=20),TD('four', width=40, height=20))
>>> tl.append(tr)
>>> tr.empty()
>>> setTDDebug(-1) # holding
holding on TDDEBUG

>>> tr.append(TD('row 2', height=69))
>>> setTDDebug(1) # resuming
resuming TDDEBUG

>>> tl.append(tr)
>>> tr.empty()
>>> tr.append(TD('row 3', height=23, colspan=3))
>>> tl.append(tr)

>>> t('setTDDebug(0)')
<table border="1" cellpadding="4" cellspacing="0" class="table" width="100%">
<tr><th class="th">Widths</th><th class="th">Heights</th></tr>
<tr><td class="td0"><table border="1" cellpadding="4" cellspacing="0" class="table" width="100%">
<tr><td class="td0"></td><td class="td0">0</td><td class="td0">1</td><td class="td0">2</td><td class="td0">3</td><td class="td0">total</td></tr>
<tr><td class="td1">0:</td><td class="td1">50</td><td class="td1">0</td><td class="td1">75%</td><td class="td1">40</td><td class="td1">90</td></tr>
<tr><td class="td0">1:</td><td class="td0">50</td><td class="td0">0</td><td class="td0">-</td><td class="td0">-</td><td class="td0">50</td></tr>
<tr><td class="td1">2:</td><td class="td1">0</td><td class="td1">0</td><td class="td1">0</td><td class="td1">-</td><td class="td1">0</td></tr>
</table></td><td class="td0"><table border="1" cellpadding="4" cellspacing="0" class="table" width="100%">
<tr><td class="td0"></td><td class="td0">0</td><td class="td0">1</td><td class="td0">2</td><td class="td0">3</td></tr>
<tr><td class="td1">0:</td><td class="td1">89</td><td class="td1">89</td><td class="td1">20</td><td class="td1">20</td></tr>
<tr><td class="td0">1:</td><td class="td0">0</td><td class="td0">0</td><td class="td0">-</td><td class="td0">-</td></tr>
<tr><td class="td1">2:</td><td class="td1">23</td><td class="td1">23</td><td class="td1">23</td><td class="td1">-</td></tr>
<tr><td class="td0"></td><td class="td0">---</td><td class="td0">---</td><td class="td0">---</td><td class="td0">---</td></tr>
<tr><td class="td1"></td><td class="td1">112</td><td class="td1">112</td><td class="td1">43</td><td class="td1">20</td></tr>
</table></td></tr>
</table>

    """
    global TDDEBUG, genCols, WIDTHS, HEIGHTS, rowNum, colNum
    if TDDEBUG == 0 and value == 0:
        print 'no debugging for tables is switched on, no switching off is required'
        return
    if value and value in (1, 2):
        if rowNum == None:
            WIDTHS = intarray()
            HEIGHTS = intarray()
            colNum = 0
        if TDDEBUG == -1:
            print 'resuming TDDEBUG'
        TDDEBUG = value
        genCols = colorGraying(startColor)
    elif value == -1:
        print 'holding on TDDEBUG'
        TDDEBUG = value
    else:
        TDDEBUG = 0
        rowNum = None
        genCols = None
        if giveTable and WIDTHS:
            wtotal = WIDTHS.totalsarray(colsum=0)
            htotal = HEIGHTS.totalsarray(rowsum=0)
            t1 = str(TABLE(body=wtotal))
            t2 = str(TABLE(body=htotal))  
            return join(TABLE(heading=["Widths", "Heights"], body =
                               [[t1, t2]], celllinebreaks=0))
def doTRDEBUGnew():
    """update rowNum and colNum when debugging table"""
    global rowNum, colNum
    if rowNum == None:
        rowNum = 0
    else:
        rowNum += 1
    colNum = 0

def doTDDEBUGnew(tdinstance):
    """update things when entering a new table data cell"""
    global colNum, WIDTHS, HEIGHTS
    self = tdinstance
    if rowNum == None:
        raise HTMLgenError('debugging table data, no TR encountered yet')
    for a in ("width", "height", "colspan", "rowspan"):
        if a in self:
            v = getattr(self, a).split('"')[1]
            try:
                exec('v%s = %s'% (a, v))
            except:
                exec('v%s = "%s"'% (a, v))
                
    if "width" not in self:
        vwidth = 0
    if "height" not in self:
        vheight = 0
        
    if "colspan" in self:
        vcolspan = int(vcolspan)
    else:
        vcolspan = 1
    if "rowspan" in self:
        vrowspan = int(vrowspan)
    else:
        vrowspan = 1
    # proceed non empty column cells:
    while WIDTHS.hasvalue(rowNum,colNum):
        colNum += 1
##            print 'starting with: colnum: %s'% colNum
    for i in range(rowNum, rowNum+vrowspan):
        for j in range(colNum, colNum+vcolspan):
            if WIDTHS.hasvalue(i,j):
                raise HTMLgenError('TDDEBUG, cell already full: %s, %s'% (i, j))
            if i == rowNum:
                HEIGHTS[i][j] = vheight
            else:
                HEIGHTS[i][j] = 0
            if j == colNum:
                WIDTHS[i][j] = vwidth
            else:
                WIDTHS[i][j] = 0





class TR(AbstractTag):
    """Define a row of cells within a table.

    This is to test if empty instances also print

    >>> tr = TR()
    >>> tr.append(TD('aaa'))
    >>> tr.append(TD())
    >>> tr.append(TD('bbb'))
    >>> str(tr)
    '\\n<tr><td>aaa</td><td></td><td>bbb</td></tr>'

    >>> tl = TableLite()
    >>> tl.append(tr)
    >>> str(tl)
    '\\n<table>\\n<tr><td>aaa</td><td></td><td>bbb</td></tr></table>'
    >>> tr = TR()
    >>> str(tl)
    '\\n<table>\\n<tr><td>aaa</td><td></td><td>bbb</td></tr></table>'
    
    
    """
    tagname = 'tr'
    attrs = ('class','id', 'style', 'align', 'bgcolor', 'bordercolor',
             'bordercolordark', 'bordercolorlight', 'nowrap', 'valign')
    def __init__(self, *contents, **kw):
        AbstractTag.__init__(self, *contents, **kw)
        if TDDEBUG > 0:
            doTRDEBUGnew()
            for c in contents:
                doTDDEBUGnew(c)

    def append(self, *items):
        """Append one or more items to the end of the container, allow for debugging"""
        AbstractTag.append(self, *items)
        if TDDEBUG > 0:

            for item in items:
                doTDDEBUGnew(item)

    def empty(self):
        if TDDEBUG > 0:
            doTRDEBUGnew()
        AbstractTag.empty(self)



class TD(AbstractTag):
    """Define a table data cell.
    
    >>> str(TD())
    '<td></td>'
    >>> str(TD(''))
    '<td></td>'

    >>> str(TD('foo'))
    '<td>foo</td>'
    >>> t = TD(' foo ')
    >>> t.append(' bar ')
    >>> t.prepend('  eggs ')
    >>> str(t)
    '<td>eggs  foo  bar</td>'

    combined with other tags:
    
    >>> str(TD(IMG('abc.jpg', width=300, height=50, alt='')))
    '<td><img src="abc.jpg" alt="" height="50" width="300"></td>'
    >>> str(TD(Href('../index.html', 'goto home')))
    '<td><a href="../index.html">goto home</a></td>'

    >>> str(TD(TD('foo'), STRONG(' bar ')))
    '<td><td>foo</td><strong>bar</strong></td>'
        

    """
    tagname = 'td'
    attrs = ('class','id', 'style', 'nowrap', 'align','valign', 'background',
             'bordercolor', 'bordercolordark', 'bordercolorlight','onclick',
             'onmouseover', 'onmouseout',
             'rowspan','colspan','height', 'width','bgcolor',
             'debugcolor')
    trailer = ''
    strip_contents = 1
    def __init__(self, *contents, **kw):
        AbstractTag.__init__(self, *contents, **kw)

    def __str__(self):
        if TDDEBUG>0:
##            print 'td, row: %s, col: %s'% (rowNum, colNum)
            self.bgcolor=genCols.next()
            s = []
            for a in ("width", "height", "colspan", "rowspan"):
                if a in self:
                    v = getattr(self, a)
                    if v:
                        s.append(v)
            if s:
                status = "status='%s'"% ''.join(s).replace('"', '')
            else:
                status = "status='no size attributes for this cell'"
            self.onmouseover = status
            self.onmouseout = "status=''"
            if TDDEBUG == 2:
                self.Class = "tiny"
                self.contents = "&nbsp;" # minimal contents
        return AbstractTag.__str__(self)
       

class TableLite(AbstractTag):
    """Container class for TH TD TR and Caption objects.

examples of the FullTable, which is a curry'd TableLite:

>>> t('FullTable()')
<table border="0" cellpadding="0" cellspacing="0" width="100%"></table>

>>> t('FullTable(border=3)')
<table border="3" cellpadding="0" cellspacing="0" width="100%"></table>

>>> t('FullTable(TR(TD("hello")), Class="test", border=2)')
<table border="2" cellpadding="0" cellspacing="0" class="test" width="100%">
<tr><td>hello</td></tr></table>

    """
    tagname = 'table'
    attrs = ('class','id', 'style', 'align', 'background', 'border',
             'bordercolor', 'bordercolordark', 'bordercolorlight',
             'cols', 'frame', 'cellpadding', 'cellspacing',
             'height', 'hspace', 'width', 'bgcolor', 'nowrap',
             'rules', 'valign', 'vspace')

FullTable = curry(TableLite, border=0, cellpadding=0, cellspacing=0, width = '100%')


class Pre(AbstractTag):
    """Render the text verbatim honoring line breaks and spacing.

    Does not escape special characters. To override this set html_escape
    to 'ON'.
    """
    tagname = 'pre'
    attrs = ('width',)
    html_escape = 'OFF'

class Strike(AbstractTag):
    """The text is struck trough with a horizontal line.
    """
    tagname = 'strike'
    attrs = ('class','id', 'style')
    trailer = ''

class Blockquote(AbstractTag):
    """Indent text as a block quotation.
    """
    tagname = 'blockquote'
    attrs = ('class','id', 'style')

Indent = Blockquote

class Big(AbstractTag):
    """Format text in a bigger font.
    """
    tagname = 'big'
    attrs = ('class','id', 'style')
    trailer = ''


class Bgsound(AbstractTagSingle):
    """Make background sound, only IE

    prefix=self.prefix can be entered in the call!    

>>> t('Bgsound("sound.wav")')
<bgsound src="sound.wav">
>>> t('Bgsound("sound.wav", loop=3)')
<bgsound loop="3" src="sound.wav">
>>> t('Bgsound(src="sound.wav", loop=5)')
<bgsound loop="5" src="sound.wav">
>>> t('Bgsound(loop=5)')
Traceback (most recent call last):
TypeError: __init__() takes exactly 2 non-keyword arguments (1 given)

>>> t('Bgsound("sound.wav", prefix="..")')
<bgsound src="../sound.wav">
    """
    tagname = 'bgsound'
    attrs = ('loop','src')
    trailer = ''
    def __init__(self, src, **kw):
        """src MUST be given as first argument

        """        
        # intercept special variable prefix:
        prefix = path('')
        if 'prefix' in kw:
            prefix = path(kw['prefix'])
            del kw['prefix']
        if type(prefix) == types.StringType:
            prefix = path(self.prefix)
            
        if src:
           if 'src' in kw:
               raise HTMLgenError('Img tag src enterd twice: "%s" and "%s"'% (`image`, kw['src']))
        self.src = prefix/src
                
        # do the standard initialise:        
        AbstractTagSingle.__init__(self, **kw)



class Font(AbstractTag):
    """Set the size or color of the text.
    """
    tagname = 'font'
    attrs = ('color', 'face', 'size')
    trailer = ''

class Address(AbstractTag):
    """A mailing address. Not a URL.
    """
    tagname = 'address'
    attrs = ('class','id', 'style')
    trailer = ''

class Emphasis(AbstractTag):
    """Format with additional emphasis. (usually italics)
    """
    tagname = 'em'
    attrs = ('class','id', 'style')
    trailer = ''

class Center(AbstractTag):
    """Center the text.
    """
    tagname = 'center'
    attrs = ()

class Cite(AbstractTag):
    """A citation.
    """
    tagname = 'cite'
    attrs = ('class','id', 'style')
    trailer = ''

class KBD(AbstractTag):
    """Keyboard-like input.
    """
    tagname = 'kbd'
    attrs = ('class','id', 'style')
    html_escape = 'OFF'

class Sample(AbstractTag):
    """Sample text. Escaping of special characters is not performed.

    To enable escaping set html_escape='ON'.
    """
    tagname = 'samp'
    attrs = ('class','id', 'style')
    html_escape = 'OFF'

class Strong(AbstractTag):
    """Generate a tag strong, for emphasising the text

>>> str(Strong(' hello, this is a test '))    
'<strong>hello, this is a test</strong>'
>>> long = ['this is the first line of long text,',' this is the second line of the long text', ' and this is the third line of the long text, together being longer than 72']
>>> str(Strong(long))
'<strong>this is the first line of long text, this is the second line of the long text and this is the third line of the long text, together being longer than 72</strong>'
>>> S = Strong('hello,')
>>> S.append(long)
>>> str(S)
'<strong>hello,this is the first line of long text, this is the second line of the long text and this is the third line of the long text, together being longer than 72</strong>'
    
    """
    strip_contents = 1
    attrs = ('class','id', 'style')
    tagname = 'strong'
    trailer = ''

class Bold(AbstractTag):
    """Generate a tag bold (normally use Strong)

    >>> str(Bold(' hello, this is a test '))    
    '<b>hello, this is a test</b>'
    
    """
    strip_contents = 1
    attrs = ('class','id', 'style')
    tagname = 'b'
    trailer = ''

B = BOLD = Bold

class Code(AbstractTag):
    """Code sample. Escaping of special characters is not performed.

    To enable escaping set html_escape='ON'.
    """
    tagname = 'code'
    attrs = ('class','id', 'style')

class Define(AbstractTag):
    """Format as definition text.
    """
    tagname = 'dfn'
    attrs = ('class','id', 'style')

class Var(AbstractTag):
    """Used for variable names.
    """
    tagname = 'var'
    attrs = ('class','id', 'style')

class Div(AbstractTag):
    """Specify a division within a document.
    """
    tagname = 'div'
    strip_contents = 1    
    attrs = ('class','id', 'style', 'align', 'lang', 'nowrap')

class TT(AbstractTag):
    """Format teletype style.
    """
    tagname = 'tt'
    attrs = ('class','id', 'style')

class U(AbstractTag):
    """Underlined text.
    """
    tagname = 'u'
    attrs = ('class','id', 'style')

class Nobr(AbstractTag):
    """Specify non-breaking text.
    """
    tagname = 'nobr'
    attrs = ()

class Small(AbstractTag):
    """Render in a smaller font.
    """
    tagname = 'small'
    attrs = ('class','id', 'style')

class Sub(AbstractTag):
    """Render as subscript.
    """
    tagname = 'sub'
    attrs = ('class','id', 'style')

class Sup(AbstractTag):
    """Render as subscript.
    """
    tagname = 'sup'
    attrs = ('class','id', 'style')

class Span(AbstractTag):
    """Generic tag to mark text for a style application.
    """
    tagname = 'span'
    attrs = ('class','id', 'style')

# XML classes, do the same as HTML classes:

class Sitemapindex(AbstractTag):
    """for sitemap index files"""
    tagname = 'sitemapindex'
    attrs = ('xmlns',)
    required_attrs = ('xmlns',)
    instance_variables = ('test','contents')      # testing validness, dangerous!
    def __str__(self):
        """if test add test options"""
        t = AbstractTag.__str__(self)
        if getattr(self, 'test', None):
            T = t.split(' ', 1)
            T.insert(1, '''xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/siteindex.xsd"\n''')
            return ' '.join(T)
        else:
            return t
        





class Sitemap(AbstractTag):
    """for sitemap one sitemap in sitemap index file"""
    tagname = 'sitemap'
    attrs = ('loc', 'lastmod', )
    required_attrs = ('loc',)
      


class Urlset(AbstractTag):
    """for sitemap starting a sitemap file"""
    tagname = 'urlset'
    attrs = ('xmlns',)
    required_attrs = ('xmlns',)
    instance_variables = ('test','contents')      # testing validness, dangerous!
    def __str__(self):
        """if test add test options"""
        t = AbstractTag.__str__(self)
        if getattr(self,'test', None):
            T = t.split(' ', 1)
            T.insert(1, '''xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd"\n''')
            return ' '.join(T)
        else:
            return t

class UrlData(AbstractTag):
    """for sitemap one entry in xms file"""
    tagname = 'url'
    attrs = ()
    required_attrs = ()



# Text Formatting Classes
class InitialCaps:
    """Utility class to process text into Initial Upper Case style
    using Font specifications. All text is converted to upper case
    and the initial characters are altered by the size given by
    the optional second argument. The rest of the characters are
    altered by the size given in the optional third argument.

    For example:

       InitialCaps('We the people', '+3', '+1')
    """
    def __init__(self, text='', upsize = '+2', downsize = '+1'):
        self.hi = Font(size=upsize)
        self.lo = Font(size=downsize)
        self.text = text
        self.upsize = upsize
        self.downsize = downsize

    def __str__(self):
        list = string.split(self.text)
        wordlist = []
        for word in list:
            word = self.hi(string.upper(word[0])) + self.lo(string.upper(word[1:]))
            wordlist.append(word)
        return string.join(wordlist)

    def __call__(self, text):
        self.text = text
        return self.__str__()

class Text:
    """Class to encapsulate text. Escape special characters for HTML.
    """
    def __init__(self, text=''):
        text = escape(text)
        self.text = str(text)

    def append(self, text=''):
        """Concatenate text characters onto the end.

        Will escape special characters.
        """
        text = escape(text)
        self.text = self.text + ' ' + str(text)

    def __str__(self):
        return self.text

class RawText:
    """Class to encapsulate raw text. Does **NOT** escape special characters.
    """
    def __init__(self, text=''):
        self.text = text

    def append(self, text):
        self.text = self.text + str(text)

    def __str__(self):
        return self.text

# ALIASES
PRE = Pre
STRONG = Strong
Italic = EM = Emphasis
Typewriter = TT

class Comment:
    """Place a comment internal to the HTML document

    Will not be visible from the browser.
    >>> str(Comment('')).strip()
    '<!--  -->'
    >>> str(Comment('abc def')).strip()
    '<!-- abc def -->'
    >>> str(Comment('-- xxx --')).strip()
    '<!-- == xxx == -->'
    
    """
    def __init__(self, text=''):
        self.text = text

    def __str__(self):
        if self.text and self.text.find('-') >= 0:
            self.text = self.text.replace('-', '=')
        return '\n<!-- %s -->\n' % self.text

    def __call__(self, text):
        self.text = text
        return self.__str__()

###### UTILITIES USED INTERNALLY ########
normalChars = string.letters + string.digits + '., -_:;(){}[]=+"' + "'"
# Make a reusable string of all characters
_allchars = string.maketrans('', '')

def makefilter(keep):
    """ Return a functor that takes a string and returns a partial copy of that
        string consisting of only the characters in 'keep'.

    """
    # Make a string of all characters that are not in 'keep'
    delchars = _allchars.translate(_allchars, keep)

    # Return the functor, binding the two strings as default args
    return lambda s, a=_allchars, d=delchars: s.translate(a, d)

filterreadable = makefilter(normalChars)

def escape(text):
    """Converts the special characters '<', '>', and '&'.

    new version QH, also changes diacritical characters
    got rid of "replace" function variable

>>> escape('')
''
>>> escape ('abc def ')
'abc def '
>>> escape('qh & ko')
'qh &amp; ko'

>>> escape('<escaping text>')
'&lt;escaping text&gt;'
>>> escape("d\xeft is a test")
'd&iuml;t is a test'
>>> escape("'quoted string'")
"'quoted string'"
>>> escape('"quoted string"')
'"quoted string"'

    """
    if not text:
        return ''
    if filterreadable(text) == text:
        return text
    TEXT = list(text)
    for i, c in enumerate (TEXT):
        if c in normalChars:
            continue
        try:
            name = codepoint2name[ord(c)]
        except KeyError:
            continue
        TEXT[i] = '&%s;'% name
    return ''.join(TEXT)


def markup_re(text, rex=None, marker=None, collapse=0):
    """Markup the contained text with a given re pattern/object with
    a given tag class instance. Uses re module.

    Arguments

        text -- string to act on
        rex -- a regular expression object or pattern from the re module which will be used
            to match all text patterns in the Paragraph body. Must have a single
            group defined. Group 1 is the matching text that will be marked.
            Defaults to all parenthetical text.
        marker -- an HTMLgen class instance to which the found text will
            be sent for wrapping (using its __call__ method). Default is Emphasis.
            Can be your function as well.
        collapse -- Optional flag. When set to 1 removes the non-
            grouped matching text from the output. Default 0.

    Returns tuple pair of the marked text and the number of matching text groups.
    """
    if rex is None: rex = re.compile('\(([^)]*)\)')
    if marker is None: marker = Emphasis()
    if type(rex) == types.StringType: rex = re.compile(rex)
    endpoints = []
    output = []
    i = 0
    count = 0
    while 1:
        # build up a list of tuples: ( 'u'|'m', begin, end )
        # 'u' indicates unmarked text and 'm' marked text
        # begin and end is the range of characters
        match = rex.search(text, i)
        if match:
            if collapse: #skip chars outside group1
                endpoints.append( ('u', i, match.start(0)) )
                i = match.end(0)
            else: #incl chars outside group1
                endpoints.append( ('u', i, match.start(1)) )
                i = match.end(1)
            endpoints.append( ('m', match.start(1), match.end(1)) ) #text2Bmarked
            count = count + 1
        else:
            endpoints.append( ('u', i, len(text) ) ) # tack on an ending slice
            break

    if count == 0: return text, 0  # didn't find any matches
    for (style, begin, end) in endpoints:
        if style == 'm':
            output.append(marker(text[begin:end]))
        else:
            output.append(text[begin:end])
    return string.join(output, ''), count


class URL:
    """Represent a Universal Resource Locator.

    Assumed to be of the form: **http://www.node.edu/directory/file.html**
    with *http* being an example protocol, *www.node.edu* being an example
    network node, *directory* being the directory path on that node, and
    *file.html* being the target filename. The argument string is parsed
    into attributes .proto , .node , .dir , .file respectively and may
    be altered individually after instantiation. The __str__ method
    simply reassembles the components into a full URL string.
    """
    def __init__(self, url):
        self.url = url
        self.parse(url)
    def parse(self, url):
        import urlparse
        self.unparse = urlparse.urlunparse
        self.proto, self.node, self.path, self.params, self.query, self.fragment = \
                    urlparse(url)
        self.dir, self.file = self.split(self.path)

    def split(self, p):
        """Same as posixpath.split()

        Copied here for availability on the Mac.
        """
        i = string.rfind(p, '/') + 1
        head, tail = p[:i], p[i:]
        if head and head != '/'*len(head):
                while head[-1] == '/':
                        head = head[:-1]
        return head, tail

    def __str__(self):
        return self.unparse( (self.proto,
                              self.node,
                              self.dir+self.file,
                              self.params,
                              self.query, self.fragment) )

    def copy(self):
        """No argument. Return a copy of this object.
        """
        return copy.deepcopy(self)



class Picture(object):
    """contains the necessary data to a picture

    fullpath: fullname
    name: short filename
    original: path of origin of file
    alt: alttext of pictur
    width, height: taken from file (Image from PIL) or from input
    label: corresponding inifile label
    

>>> folderName = path('D:/projects/sitegen/testpicture')

    call with path argument, no specs only filename:
>>> p = Picture(folderName/'aap.jpg')
>>> print p.width, p.height
92 128

    call width width and height (no control BTW)
>>> q = Picture(folderName/'noot.jpg', width=105, height = 103)
>>> print q.width, q.height
105 103

call with direct string as filename:
>>> p = Picture(r'D:\\projects\\sitegen\\testpicture\\mies.jpg', label='label van picture')
>>> print p.width, p.height
76 97
>>> print p.fullpath, p.name
D:\\projects\\sitegen\\testpicture\\mies.jpg mies.jpg

>>> print 'label:', p.label
label: label van picture
>>> print 'with indexes: %s, %s, %s'% (p[0], p[1], p[2])
with indexes: mies.jpg, 76, 97


>>> print 'wrong index: ', p[3]
Traceback (most recent call last):
HTMLgenError: Picture object cannot be indexed with: 3

    """
    def __init__(self, fullpath, **kw):
        """Minimal is the complete path

        Can also contain width, height, alttext, label (originating from in inifile)

        """
        for k in kw:
            setattr(self, k, kw[k])
            
        pName = path(fullpath)
        self.fullpath = pName.normpath() # return strings!
        self.name = pName.basename()
        if not (hasattr(self, 'width') and hasattr(self, 'height')):
            ima = Image.open(self.fullpath)
            self.width = ima.size[0]
            self.height = ima.size[1]
        for l in ('label', 'original', 'alt'):
            if not hasattr(self, l):
                setattr(self, l, '') # to be present!
        
    def __getitem__(self, i):
        """special for 0, 1, 2, return .name, .width, .height"""
        if i == 0:
            return self.name
        elif i == 1:
            return self.width
        elif i == 2:
            return self.height
        else:
            raise HTMLgenError('Picture object cannot be indexed with: %s'% i)

            
   

def escapeJSString(s):
    """insert backslashes before " in strings

    if string is already inside "..." these are kept.
    note: in examples a single "\" is represented by "\\\\" ! 

    >>> a = escapeJSString('abc')
    >>> a, len(a)
    ('abc', 3)
    
    >>> a = escapeJSString('a"bc')
    >>> a, len(a)
    ('a\\\\"bc', 5)
    >>> a = escapeJSString('""abc')
    >>> a, len(a)
    ('\\\\"\\\\"abc', 7)
    >>> a = escapeJSString('"a"abc"')
    >>> a, len(a)
    ('"a\\\\"abc"', 8)
    >>> a = escapeJSString("'")
    >>> a, len(a)
    ("'", 1)
    >>> a = escapeJSString("a ' bvc")
    >>> a, len(a)
    ("a ' bvc", 7)
    >>> a = escapeJSString("a-b.txt")
    >>> a, len(a)
    ('a-b.txt', 7)

    """
    if s.startswith('"') and s.endswith('"'):
        s = '"' + s[1:-1].replace('"', '\\"') + '"'
    else: 
        s = s.replace('"', '\\"')
    return s

def mpath(path):
    """Converts a POSIX path to an equivalent Macintosh path.

    Works for ./x ../x /x and bare pathnames.
    Won't work for '../../style/paths'.

    Also will expand environment variables and Cshell tilde
    notation if running on a POSIX platform.
    """
    import os
    if os.name == 'mac' : #I'm on a Mac
        if path[:3] == '../': #parent
            mp = '::'
            path = path[3:]
        elif path[:2] == './': #relative
            mp = ':'
            path = path[2:]
        elif path[0] == '/': #absolute
            mp = ''
            path = path[1:]
        else: # bare relative
            mp = ''
        pl = string.split(path, '/')
        mp = mp + string.join(pl, ':')
        return mp
    elif os.name == 'posix': # Expand Unix variables
        if path[0] == '~' :
            path = os.path.expanduser( path )
        if '$' in path:
            path = os.path.expandvars( path )
        return path
    else: # needs to take care of dos & nt someday
        return path

def writestring2fileifdifferent(s, filename):
    """if file exists, compare, otherwise write anyway"""
##    filename = mpath(filename)
    if os.path.exists(filename):
        if compare_s2f(s, filename):
            f = open(filename, 'w')
            f.write(s)
            f.close()
            if PRINTECHO: print 'rewrote: "'+filename+'"'
            return 1
        else:
            if PRINTECHO > 1: print 'file unchanged: "'+filename+'"'
    else:
        f = open(filename, 'w')
        f.write(s)
        f.close()
        if PRINTECHO: print 'wrote new: "'+filename+'"'
        return 1

def compare_f2f(f1, f2):
    """Helper to compare two files, return 0 if they are equal."""

    BUFSIZE = 8192
    fp1 = open(f1)
    try:
        fp2 = open(f2)
        try:
            while 1:
                b1 = fp1.read(BUFSIZE)
                b2 = fp2.read(BUFSIZE)
                if not b1 and not b2: return 0
                c = cmp(b1, b2)
                if c:
                    return c
        finally:
            fp2.close()
    finally:
        fp1.close()

def getIdFromFrom(fromtext):
    """in left menu get name of page only from linkTo"""
    one = str(fromtext).split('/')[-1]
    two = one.split('.')[0]
    return two

def compare_s2f(s, f2):
    """Helper to compare a string to a file, return 0 if they are equal."""

    BUFSIZE = 8192
    i = 0
    fp2 = open(f2)
    try:
        while 1:
            try:
                b1 = s[i: i + BUFSIZE]
                i = i + BUFSIZE
            except IndexError:
                b1 = ''
            b2 = fp2.read(BUFSIZE)
            if not b1 and not b2: return 0
            c = cmp(b1, b2)
            if c:
                return c
    finally:
        fp2.close()

def join(*Input, **kw):
    """join all things (recursively if necessary)
    keyword arguments currently only "separator", default being ''
    
>>> t("join(['a', 'b'])")
ab
>>> join()
''
>>> join(None, 'ab', (TD(), TD('a'), TD('b'), None), 'cd')
'ab<td></td><td>a</td><td>b</td>cd'
>>> join([['a', TD(), 'b', None], ['c', [TD('aaa'), TD('bbb')], None, 'd'], 'e'])
'a<td></td>bc<td>aaa</td><td>bbb</td>de'

    """
    separator = kw.get('separator', '')
    if len(Input) == 0:
        return ''
    elif len(Input) > 1:
        S=[]
        for i in Input:
            S.append(join(i, separator=separator))
        return separator.join(S)

    input = Input[0]           
    if input == None:
        return separator
    elif type(input) == types.StringType:
        if input == "None":
            return separator
        else:
            return input + separator
    elif type(input) == types.ListType or type(input) == types.TupleType:
        return separator.join(map(join, input))
    else:
        return str(input)+separator
    
# t and T for testing
# results are recorded to file "testName"
testTxt = r'testHTMLgen.txt'
testHtml = r'testHTMLgen.html'
testStylesheet = r'test.css'
testJSfile = r'test.js'
testDir = path('d:/projects/sitegen/testHTMLgen')

def t(com):
    """do a line of HTMLgen code, and print. replace multiple newlines"""
    res = eval(com)
    res = str(res).replace('\n\n', '\n').strip()
    print res

def T(com):
    """do a line of HTMLgen code, log to testDoc and print result"""
    res = eval(com)
    resstrip = str(res).replace('\n\n', '\n').strip()
    print resstrip
    f = open(testDir/testTxt, 'a')
    f.write(str(Div(Paragraph(escape(str(com))), Class="command")))
    resList = str(res).split('\n')
    f.write(str(Div(Paragraph('<br>'.join(map(escape, resList))), Class="result")))
    f.write(str(res)+'\n\n')
    f.write(str(HR()))
    
  
def _test():
    global testDoc
    import doctest, HTMLgen
    reload(HTMLgen)
    f = open(testDir/testTxt, 'w')
    
    doctest.master = None
    doctest.testmod(HTMLgen)
    t = open(testDir/testTxt).read()
    if not t:
        print 'geen HTML test uitvoer'
        return
    d = SimpleDocument()
    d.stylesheet = "test.css"
    d.script = Script(src=testJSfile, type="text/javascript")
    d.append(t)
    d.write(testDir/testHtml)
    print 'test uitvoer in document: %s'% testHtml
    
if __name__ == "__main__":
    _test()
