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
# (C) 2001, National Research Council of Canada
#
##############################################################################

"""Support services for [SourceBuff] subclasses.

This module defines a series of services that can be used by various
[SourceBuff] subclasses to implement concrete methods.

The concrete behaviour is factored into services as opposed to being
implemented directly as methods of [SourceBuff] subclasses. This is to
allow more flexible mixin of implementation of behaviours than can be
supported through subclassing.

For example, class SourceBuffX may need to implement some methods like
class SourceBuffY, but other methods like class SourceBuffZ. With
subclassing, SourceBuffX would have to derive from both SourceBuffY and
SourceBuffZ, which means that either SourceBuffY would have to derive from
SourceBuffZ or vice-versa (can't use multiple inheritance to make
SourceBuffX inherit from both SourceBuffY and SourceBuffZ because that would
result in method name clashes). But maybe SourceBuffY and SourceBuffZ need
to be on different branches of the inheritance hierarchy.

By implementing variants of a given methods as variants of a given
service, we make it possible for two classes to use the same variant
of a method even if they are not on the same branch of the inheritance
hierarchy. Without such an approach, the code implementing that
variant would have to be cloned and copied to the two classes.


..[SourceBuff] file:///./SourceBuff.SourceBuff.html"""

import Object
import re
import debug

import SourceBuffState
import sr_interface

class ClientSideImplementation:
   """Mixin class for client side implementations of SB_Services."""
 
   def __init__(self, **args_super):
        self.deep_construct(ClientSideImplementation, 
                            {}, 
                            args_super, 
                            {}) 
   
   def talk_msgr(self):
       return self.buff.app.talk_msgr

class SB_Service(Object.OwnerObject):
    """Support service for SourceBuff classes.

    Class for defining some sort of service that can be used by various
    [SourceBuff] subclasses to implement concrete methods.

    For more details on *SB_Service*'s reason for existence,
    consult documentation of the [sb_services] module.
    
    **INSTANCE ATTRIBUTES**
    
    [SourceBuff] *buff* -- The buffer for which we are providing the service.

    CLASS ATTRIBUTES**
    
    *none* -- 

    ..[SourceBuff] file:///./SourceBuff.SourceBuff.html
    ..[sb_services] file:///./sb_services.sb_services.html"""
    
    def __init__(self, buff, **args_super):
        self.deep_construct(SB_Service, 
                            {'buff': buff}, 
                            args_super, 
                            {})
        self.name_parent('buff')

class SB_ServiceLang(SB_Service):

    """Provides services for determining the programming language of a
    particular buffer.

    This service is implemented completely at the VoiceCode level so
    that it can be used by [SourceBuff] classes for editors that are
    not language aware.
    
    **INSTANCE ATTRIBUTES**
    
    *none*-- 
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, **args_super):
        self.init_attrs({})
        self.deep_construct(SB_ServiceLang, 
                            {}, 
                            args_super, 
                            {})

    def language_name(self):
        """Returns the name of the language a file is written in
        
        **INPUTS**        

        *none*

        **OUTPUTS**

        *STR* -- the name of the language
        """
        debug.virtual('SB_ServiceLang.language_name')

    def file_language_name(self, file_name):                
        debug.virtual('SB_ServiceLang.file_language_name')
        
class SB_ServiceLangServerSide(SB_ServiceLang):

    """Provides services for determining the programming language of a
    particular buffer.

    This service is implemented completely at the VoiceCode level so
    that it can be used by [SourceBuff] classes for editors that are
    not language aware.
    
    **INSTANCE ATTRIBUTES**
    
    *none*-- 
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, language_names=None, **args_super):
        self.init_attrs({'file_language': {'c': 'C', 'h': 'C', 'C': 'C', 'cpp': 'C', 'cc' : 'C', 'py': 'python'}})
        self.deep_construct(SB_ServiceLang, 
                            {'language_names': language_names}, 
                            args_super, 
                            {})

    def language_name(self):
        """Returns the name of the language a file is written in
        
        **INPUTS**        

        *none*

        **OUTPUTS**

        *STR* -- the name of the language
        """

        language = None
        file_name = self.buff.file_name()
        debug.trace('SB_ServiceLang.language_name', 
            'file_name=%s, self.buff.name()=%s' % (file_name, self.buff.name()))
        if file_name != None:
            language = self.file_language_name(file_name)
        return language

    def file_language_name(self, file_name):
        language = None
        a_match = re.match('^.*?\.([^\.]*)$', file_name)
        extension = ""
        if a_match:
            extension = a_match.group(1)

#        print 'extension is "%s"' % extension
        if self.file_language.has_key(extension):
            language =  self.file_language[extension]
#        print 'language is "%s"' % language
        return language


class SB_ServiceLangMessaging(SB_ServiceLang, ClientSideImplementation):

    """Provides services for determining the programming language of a
    particular buffer.

    This version of the service is implemented at the client level.
    
    **INSTANCE ATTRIBUTES**
    
    *none*-- 
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, **args_super):
        self.deep_construct(SB_ServiceLangMessaging, 
                            {}, 
                            args_super, 
                            {})
        

    def language_name(self):
        """Returns the name of the language a file is written in
        
        **INPUTS**        

        *none*

        **OUTPUTS**

        *STR* -- the name of the language
        """
        self.talk_msgr().send_mess('language_name', 
            {'buff_name': self.buff.name()})
        response = self.talk_msgr().get_mess(expect=['language_name_resp'])
        debug.trace('SB_ServiceLangMessaging.language_name', 
            'returning %s' % response[1]['value'])
        return response[1]['value']
        
    def file_language_name(self, file_name):                
        self.talk_msgr().send_mess('file_language_name', 
            {'file_name': file_name})
        response = self.talk_msgr().get_mess(expect=['file_language_name_resp'])
        return response[1]['value']
        

class SB_ServiceLineManip(SB_Service):
        
    """Provides line numbering services.

    Some [SourceBuff] subclasses may decide to use the editor's
    line manipulation capabilities instead of the ones provided by
    this class.
    
    **INSTANCE ATTRIBUTES**
    
    *none*-- 
    
    CLASS ATTRIBUTES**
    
    *none* -- 

    ..[SourceBuff] file:///./SourceBuff.SourceBuff.html"""
                        
    def __init__(self, **args_super):
        self.deep_construct(SB_ServiceLineManip, 
                            {}, 
                            args_super, 
                            {})

    def line_num_of(self, position = None):
        """
        Returns the line number for a particular cursor position
        
        **INPUTS**
        
        *INT* position -- The position.  (defaults to the current position)
        
        **OUTPUTS**
        
        *INT line_num* -- The line number of that position
        """
        
        #
        # Make sure the position is within range
        #
        debug.trace('SB_ServiceLineManip.line_num_of',
            'position = %s' % position)
        if position == None:
            position = self.buff.cur_pos()
            position = self.buff.make_within_range(position)

        
        #
        # Find line number of position
        #
        regexp = re.compile('($|%s)' % self.buff.newline_regexp())
        line_start_pos = 0
        line_num = 1
        curr_line = 1
#        print self.buff.len()
        debug.trace('SB_ServiceLineManip.line_num_of',
            'position = %s' % position)
        length = self.buff.len()
        while (1):
            a_match = regexp.search(self.buff.contents(), line_start_pos)
            if not a_match:
                break
            else:
                line_end_pos = a_match.start()
#                if line_end_pos < 10:
#                    debug.trace('SB_ServiceLineManip.line_num_of',
#                        'found line end at %d' % line_end_pos)
                if position >= line_start_pos and position <= line_end_pos:
                    line_num = curr_line
                    break
                if a_match.end() == a_match.start():
# don't want to get stuck if we have a zero-length match (i.e. to $)
                    line_start_pos = a_match.start() + 1
                else:
# if newline_regexp includes \r\n and \n, we don't want to see \r\n, 
# match \r\n, then advance the start of the search by one and match \n as
# an additional new line.
                    line_start_pos = a_match.end()
                if line_start_pos > length:
                    break
                curr_line = curr_line + 1                            
        
        debug.trace('SB_ServiceLineManip.line_num_of',
            'returning %d' % line_num)
        return line_num


    def line_num_of_old(self, position = None):
        """
        Returns the line number for a particular cursor position
        
        **INPUTS**
        
        *INT* position -- The position.  (defaults to the current position)
        
        **OUTPUTS**
        
        *INT line_num* -- The line number of that position
        """
        
        #
        # Make sure the position is within range
        #
        if position == None:
            position = self.buffcur_pos()
        position = self.buff.make_within_range(position)
        
        #
        # Find line number of position
        #
        regexp = re.compile(self.buff.newline_regexp())
        lines = regexp.split(self.buff.contents())
        line_start_pos = None
        line_end_pos = 0
        line_num = 1
        curr_line = 0
        for a_line in lines:
            curr_line = curr_line + 1
            line_start_pos = line_end_pos
            line_end_pos = line_end_pos + len(a_line) 
            if position >= line_start_pos and position < line_end_pos:
                line_num = curr_line
                break
            
        return line_num
        


    def number_lines(self, astring, startnum=1):
        """Assign numbers to lines in a string.

        Used mainly for the purpose of doing a printout of the buffer
        content around the cursor (usually during regression testing).

        *STR astring* is the string in question.

        *INT startnum* is the number of the first line in *astring*
        
        Returns a list of pairs *[(INT, STR)]* where first entry is
        the line number and the second entry is the line."""

        #
        # Note: need to split using regexp self.buff.newline_regexp()
        #       but for now this will do.
        #

        regexp = re.compile(self.buff.newline_regexp())
        lines = regexp.split(astring)
        result = []

        if (astring != ''):
            lineno = startnum
#            if (if re.match(self.buff.newline_regexp(), astring):
#                lineno = startnum + 1
#  this would make all lines off by 1
                
            for aline in lines:
                result[len(result):] = [(lineno, aline)]
                lineno = lineno + 1
            
        return result


    def beginning_of_line(self, pos = None):
        """Returns the position of the beginning of line at position *pos*
        
        **INPUTS**
        
        *INT* pos -- Position for which we want to know the beginning of
        line, or None for the current line.
        

        **OUTPUTS**
        
        *INT* beg_pos -- Position of the beginning of the line
        """

#        print '-- SB_ServiceLineManip.beginning_of_line: pos=%s' % pos
        
        if pos is None:
            pos = self.buff.cur_pos()
        contents = self.buff.contents()        
        from_pos = 0
        regexp = re.compile(self.buff.newline_regexp())

        #
        # Find all occurences of a newline and chose the one closest
        # to the cursor
        #
        closest = 0
        closest_dist = pos
        from_pos = 0
        while 1:            
            a_match = regexp.search(contents, pos=from_pos)
            if a_match:
                dist = pos - a_match.end()
                if dist < 0:
                    break
                if dist < closest_dist:
                    closest = a_match.end()
                    closest_dist = dist
                from_pos = a_match.end()
            else:
                #
                # No more matches
                #
                break                

#            print '-- SB_ServiceLineManip.beginning_of_line: returning closest=%s' % closest
            
        return closest

    def end_of_line(self, pos = None):
        """Returns the position of the end of line at position *pos*
        
        **INPUTS**
        
        *INT* pos -- Position for which we want to know the end of
        line, or None for the current line.
        

        **OUTPUTS**
        
        *INT* end_pos -- Position of the end of the line
        """

#        print 'pos ', pos
        if pos is None:
            pos = self.buff.cur_pos()
#        print 'pos ', pos
        contents = self.buff.contents()
        regexp = re.compile('$|(%s)' % self.buff.newline_regexp())
        a_match = regexp.search(contents, pos)
#        print 'a_match ', repr(a_match), a_match.group(0)
        return a_match.start()

    def goto_line(self, linenum, where=-1):
        """Go to a particular line in a buffer.

        *INT linenum* is the line number.

        *INT where* indicates if the cursor should go at the end
         (*where > 0*) or at the beginning (*where < 0*) of the line.
        """
        self.buff.goto(0)
        ii = 1; found = 1
        while (ii < linenum and found):
            found = self.buff.search_for('\n', 1)
            ii = ii + 1
        if (where > 0):
            found = self.buff.search_for('\n', 1)
            if not found:
                self.buff.goto(self.buff.len())
                

class SB_ServiceIndent(SB_Service):
        
    """Provides code indentation services.

    Some [SourceBuff] subclasses may decide to use the editor's
    code indentation  capabilities instead of the ones provided by
    this class.
    
    **INSTANCE ATTRIBUTES**
    
    *INT* indent_level=None -- If not *None*, the mediator will replace tab
    characters by this number of spaces before inserting code in a source
    buffer.

    *BOOL* indent_to_curr_level=None -- If true, then when the mediator inserts
    code, it will indent it to the level of the insertion point.
    
    
    CLASS ATTRIBUTES**
    
    *none* -- 

    ..[SourceBuff] file:///./SourceBuff.SourceBuff.html"""

    def __init__(self, indent_level, indent_to_curr_level=None,
                 **attrs):
        self.deep_construct(SB_ServiceIndent,
                            {'indent_level': indent_level,
                             'indent_to_curr_level': indent_to_curr_level},
                            attrs
                            )                                                
    
    def insert_indent(self, code_bef, code_after, range = None):
        
        """Insert code into source buffer and indent it. We use Mediator
        level indentation functionality as opposed to Editor level one. More
        precisely:

        - code is indented at the sane level as cursor position *range[0]*
        - tab characters inside the code are replaced by *self.indent_level*
        blanks

        Replace code in range with the concatenation of code *STR
        code_bef* and *str code_after*. Cursor is put right after *code_bef*.

        **INPUTS**

        *STR* code_bef -- code to be inserted before new cursor location
        
        *STR* code_bef -- code to be inserted after new cursor location

        *(INT, INT)* range -- code range to be replaced.  If None,
        defaults to the current selection.

        **OUTPUTS**

        *none*
        """

        debug.trace('SB_ServiceIndent.insert_indent',
            'code_bef="%s", code_after="%s", range=%s' \
            % (code_bef, code_after, repr(range)))
        
        if range == None:
            range = self.buff.get_selection()
        range = self.buff.make_valid_range(range)

        debug.trace('SB_ServiceIndent.insert_indent',
            'range=%s' % repr(range))


        #
        # Carry out mediator level indentation
        #
        # First replace tabs by appropriate number of space
        #
        if self.indent_level != None:
            code_bef = self.replace_tabs(code_bef)
            code_after = self.replace_tabs(code_after)
            debug.trace('SB_ServiceIndent.insert_indent',
                    'after replacing tabs with spaces, code_bef="%s", code_after="%s"' % (code_bef, code_after))
            

        #
        # Then indent the code by the number of blanks before the
        # insertion point.
        #        
        if self.indent_to_curr_level:
            curr_level = self.indentation_at(range[0])
            debug.trace('SB_ServiceIndent.insert_indent',
                'code_bef was %s' % repr(code_bef))
            code_bef = self.indent_by_spaces(code_bef, curr_level)
            debug.trace('SB_ServiceIndent.insert_indent',
                'code_bef now %s' % repr(code_bef))
            debug.trace('SB_ServiceIndent.insert_indent',
                'code_after was %s' % repr(code_after))
            code_after = self.indent_by_spaces(code_after, curr_level)
            debug.trace('SB_ServiceIndent.insert_indent',
                'code_after now %s' % repr(code_after))

        #
        # Now insert the code
        #

        inserted = None
        appended = None
        if code_bef:
            inserted = self.buff.insert(code_bef, range)
        pos = self.buff.cur_pos()
        if code_after:
            appended = self.buff.insert(code_after)
            self.buff.goto(pos)
        return inserted, appended

    def incr_indent_level(self, levels=1, range=None):
        
        """Increase the indentation of a region of code by a certain number of
        levels. This version uses Mediator level indentation functionality
        as opposed to Editor level one.
        
        **INPUTS**
        
        *INT* levels=1 -- Number of levels to indent by.
        
        *(INT, INT)* range=None -- Region of code to be indented 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        #
        # AD: Note the use of *code_range* instead of *range* for the argument
        # name. Otherwise, it conflicts with the range() function in
        # for loops below.
        #
        # DCF: Unless we want to rename this consistently for all
        # indentation methods, a better workaround is to 
        # use __builtins__.range when you mean the builtin function
        # However, I don't see any reference to the range function below, so it
        # seems this is no longer necessary in this method
        #
        if range == None:
            range = self.buff.get_selection()
        range = self.buff.make_valid_range(range)

        #
        # Indent from start of first line in range
        #
        start = self.buff.beginning_of_line(range[0]) - 1
        if start < 0: start = 0
        end = range[1]
        code_to_indent = self.buff.contents()[start:end]

        #
        # Indent the code
        #
        num_spaces = levels * self.indent_level
        indented_code = self.indent_by_spaces(code_to_indent, num_spaces)

        self.buff.delete((start, end))
        self.buff.goto(start)
        self.buff.insert(indented_code)
        

    def decr_indent_level(self, levels=1, range=None):

        """Decrease the indentation of a region of code by a certain number of
        levels. This version uses Mediator level indentation functionality
        as opposed to Editor level one.        
        
        **INPUTS**
        
        *STR* levels=1 -- Number of levels to unindent

        *(INT, INT)* range=None -- Start and end position of code
        to be indent.  If *None*, use current selection

        **OUTPUTS**
        
        *none* -- 
        """
        
        if range == None:
            range = self.buff.get_selection()
        range = self.buff.make_valid_range(range)

        #
        # Unindent from start of first line in range
        #
        start = self.buff.beginning_of_line(range[0])
        end = range[1]

        code_to_unindent = self.buff.contents()[start:end]

        #
        # Unindent the code using a regexp
        #
        regexp = '(^|%s) {0,%s}' % (self.buff.newline_regexp(), levels * self.indent_level)
        unindented_code = re.sub(regexp, '\\1', code_to_unindent)
        
        self.buff.delete((start, end))
        self.buff.goto(start)
        self.buff.insert(unindented_code)
        

    def replace_tabs(self, code):
        """Replaces tabs in a piece of code, by the appropriate number of
        spaces. This is done solely by the mediator, i.e. we do not invoke
        the editor's indentation capabilities.
        
        **INPUTS**
        
        *STR* code -- Code for which we want to replace tabs.
        

        **OUTPUTS**
        
        *STR* fixed_code -- The code with tabs replaced.
        """

        fixed_code = code
        if self.indent_level != None:
            spaces = ' ' * self.indent_level
            fixed_code = re.sub('\t', spaces, fixed_code)
        return fixed_code


    def indent_by_spaces(self, code, num_spaces):
        """Indents each line in *code* by *num_spaces* blanks.
        
        **INPUTS**
        
        *STR* code -- code to be indented. 
        
        *INT* num_spaces -- number of spaces to indent the code by 
        

        **OUTPUTS**
        
        *STR* indented_code -- Code after indentation.
        """

        indented_code = code
        spaces = ' ' * num_spaces
        indented_code = re.sub(self.buff.newline_regexp(), self.buff.pref_newline_convention() + spaces, indented_code)
        return indented_code


    def indentation_at(self, pos):

        """Determines the indentation level (in number of spaces) of
        line at position *pos*.
        
        **INPUTS**
        
        *INT* pos -- Position at which we want to know indentation.
        

        **OUTPUTS**
        
        *INT* num_spaces -- Number of spaces before the start of line at *pos*
        """
        
        content = self.buff.contents()
        original_pos = pos
        pos_nonblank = pos
        debug.trace('SB_ServiceIndent.indentation_at',
            'len(content) = %d, pos = %d' % (len(content), pos))

        #
        # Go back from pos until we meet a newline character. Remember position
        # of closest non blank.
        #
        while pos > 0:
            pos = pos - 1
            if re.match(self.buff.newline_regexp(), content[pos]):
                pos = pos + 1
                break
            if not re.match('\s', content[pos]):
                pos_nonblank = pos

        #
        # Compute number of spaces. 
        #
        num_spaces = pos_nonblank - pos
        debug.trace('SB_ServiceIndent.indentation_at',
            'line through pos was %s' % repr(content[pos:original_pos+1]))
        debug.trace('SB_ServiceIndent.indentation_at',
            'num_spaces was %d' % num_spaces)
        
        return num_spaces


class SB_ServiceState(SB_Service):

    """Provides services for saving and restoring the contents of a
    buffer and comparing them with the current state.

    **INSTANCE ATTRIBUTES**
    
    *none*-- 
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, **args_super):
        self.deep_construct(SB_Service, 
                            {}, 
                            args_super, 
                            {})

    def _state_cookie_class(self):
        """returns the class object for the type of cookie used by
        store_current_state.

        **INPUTS**

        *none*

        **OUTPUTS**

        *CLASS* -- class of state cookies corresponding to this
        SB_ServiceState

        """
        debug.virtual('SB_ServiceState._state_cookie_class')
        
    def store_current_state(self):
        """stores the current state of the buffer, including both the
        contents and the current selection, for subsequent restoration.
        Store_current_state returns a "cookie" which can be passed to
        restore_state or compare_with_current.  The type and attributes
        of the cookie will depend on the specific subclass of
        SB_ServiceState.  In the most straightforward implementation, it 
        may include a copy of the entire contents of the
        buffer and the selection.  In other cases, particularly when the
        editor or SB_ServiceState provides an internal undo stack, it may simply be a
        reference to a point in this stack.
        
        Important Notes:
        
        You should only pass the cookie to methods of
        the SAME SourceBuff object from which it came.  Generally,
        cookies can not be pickled and retrieved.

        The type of cookie will vary with the concrete subclass 
        of SB_ServiceState.  The corresponding class object is 
        returned by _state_cookie_class.  However, external callers
        should not depend on the type, attributes, or methods 
        of the cookie.

        **INPUTS**

        *none*

        **OUTPUTS**

        *SourceBuffCookie* -- state cookie (see above).  Note that
        SourceBuffCookie is a dummy class.  The
        actual return type will vary with SB_ServiceState subclass.
        """
        debug.virtual('SB_ServiceState.store_current_state')

    def restore_state(self, cookie):
        """restores the buffer to its state at the time when
        the cookie was returned by store_current_state.  Both the
        contents and the selection will be restored.  However, other
        data, such as the search history, may not.  The restore
        operation can fail, which will be indicated by a return value of
        0, so the caller should always check the return value.
        
        **INPUTS**

        *SourceBuffCookie cookie* -- see above.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SB_ServiceState subclass.

        **OUTPUTS**

        *BOOL* -- true if restore was successful

        """
        debug.virtual('SB_ServiceState.restore_state')
      
    def compare_states(self, first_cookie, second_cookie, selection = 0):
        """compares the buffer states at the times when
        two cookies were returned by store_current_state.  By default,
        only the buffer contents are compared, not the selection, unless
        selection == 1.  If the state corresponding to either cookie has
        been lost, compare_states will return false.

        **INPUTS**

        *SourceBuffCookie* first_cookie, second_cookie -- see 
        store_current_state.  Note that SourceBuffCookie is a dummy 
        type, not an actual class.  The actual type will vary with 
        SourceBuff subclass.

        *BOOL* selection -- compare selection as well as contents

        **OUTPUTS**

        *BOOL* -- true if states are the same, false if they are not, or
        it cannot be determined due to expiration of either cookie
        """
        debug.virtual('SB_ServiceState.compare_states')

    def compare_state_selections(self, first_cookie, second_cookie):
        """compares the selection and cursor positions at the times when
        two cookies were returned by store_current_state.
        If the state corresponding to either cookie has
        been lost, compare_states will return false.

        This method does not synchronize with the editor prior to
        comparing with "current".  To ensure that the "current" state 
        is really current, the caller must synchronize.
        (This avoids having duplicate synchronize calls 
        when comparing with the current state of more than one buffer).

        **INPUTS**

        *SourceBuffCookie* first_cookie, second_cookie -- see 
        store_current_state.  Note that SourceBuffCookie is a dummy 
        type, not an actual class.  The actual type will vary with 
        SourceBuff subclass.

        **OUTPUTS**

        *BOOL* -- true if position and selection are the same, false if 
        they are not, or it cannot be determined due to expiration of 
        either cookie
        """
        debug.virtual('SB_ServiceState.compare_state_selections')

    def compare_with_current(self, cookie, selection = 0):
        """compares the current buffer state to its state at the time when
        the cookie was returned by store_current_state.  By default,
        only the buffer contents are compared, not the selection, unless
        selection == 1.  If the state corresponding to the cookie has
        been lost, compare_with_current will return false.

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SB_ServiceState subclass.

        *BOOL* selection -- compare selection as well as contents

        **OUTPUTS**

        *BOOL* -- true if state is the same, false if it is not, or
        it cannot be determined due to expiration of the cookie
        """
        debug.virtual('SB_ServiceState.compare_with_current')

    def compare_selection_with_current(self, cookie):
        """compares the current buffer position and selection to these
        values at the time when the cookie was returned by 
        store_current_state.  If the state corresponding to the cookie has
        been lost, compare_with_current will return false.

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        **OUTPUTS**

        *BOOL* -- true if position and selection are the same, false if 
        they are not, or if it cannot be determined due to 
        expiration of the cookie
        """
        debug.virtual('SB_ServiceState.compare_selection_with_current')

    def get_state_pos_selection(self, cookie):
        """retrieves the position and selection from a given state
        cookie.  

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        **OUTPUTS**

        *(INT, (INT, INT))* -- position and selection at the time the
        cookie was created by store_current_state, or None if the cookie
        is invalid (usually because the state corresponding to the cookie 
        has been lost).
        """
        debug.virtual('SB_ServiceState.get_state_pos_selection')


    def valid_cookie(self, cookie):
        """checks whether a state cookie is valid or expired.
        If the state corresponding to the cookie has
        been lost, valid_cookie will return false.

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SB_ServiceState subclass.

        **OUTPUTS**

        *BOOL* -- true if cookie is valid (i.e. restore_state should be
        able to work)
        """
        debug.virtual('SB_ServiceState.valid_cookie')

class SB_ServiceFullState(SB_ServiceState):

    """Provides services for saving and restoring the contents of a
    buffer and comparing them with the current state.

    **INSTANCE ATTRIBUTES**
    
    *none*-- 
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, **args_super):
        self.deep_construct(SB_ServiceState, 
                            {}, 
                            args_super, 
                            {})


    def _state_cookie_class(self):
        """returns the class object for the type of cookie used by
        store_current_state.

        **INPUTS**

        *none*

        **OUTPUTS**

        *CLASS* -- class of state cookies corresponding to this
        SourceBuff

        """
        return SourceBuffState.SourceBuffState
        
    def store_current_state(self):
        """stores the current state of the buffer, including both the
        contents and the current selection, for subsequent restoration.
        Store_current_state returns a "cookie" which can be passed to
        restore_state or compare_with_current.  The type and attributes
        of the cookie will depend on the specific subclass of
        SourceBuff.  In the most straightforward implementation, it 
        may include a copy of the entire contents of the
        buffer and the selection.  In other cases, particularly when the
        editor or SourceBuff provides an internal undo stack, it may simply be a
        reference to a point in this stack.
        
        Important Notes:
        
        You should only pass the cookie to methods of
        the SAME SourceBuff object from which it came.  Generally,
        cookies can not be pickled and retrieved.

        The type of cookie will vary with the concrete subclass 
        of SourceBuff.  The corresponding class object is 
        returned by _state_cookie_class.  However, external callers
        should not depend on the type, attributes, or methods 
        of the cookie.

        **INPUTS**

        *none*

        **OUTPUTS**

        *SourceBuffState* -- state cookie (see above)
        """
        selection = self.buff.get_selection()
        pos = self.buff.cur_pos()
        if pos == selection[0]:
            cursor_at = 0
        else:
            cursor_at = 1
        debug.trace('SB_ServiceFullState.store_current_state',
            'buffer %s' % self.buff.name())
#        debug.trace('SB_ServiceFullState.store_current_state',
#            '%s: contents = \n%s\n' % (self.buff.name(), self.buff.contents()))
        if debug.trace_is_active('SB_ServiceFullState.store_current_state'):
            contents = self.buff.contents()
            first_lines = re.match(r'.*\n.*\n', contents).group()
            last_lines = re.search(r'\n.*\n.*\n?$', contents).group()
        cookie = SourceBuffState.SourceBuffState(buff_name = self.buff.name(), 
            contents = self.buff.contents(), 
            selection = selection, cursor_at = cursor_at,
            last_search = self.buff.last_search)
        return cookie

    def restore_state(self, cookie):
        """
        restores the buffer to its state at the time when
        the cookie was returned by store_current_state.  Both the
        contents and the selection will be restored.  However, other
        data, such as the search history, may not.  The restore
        operation can fail, which will be indicated by a return value of
        0, so the caller should always check the return value.
        
        **INPUTS**

        *SourceBuffState cookie* -- see above.

        **OUTPUTS**

        *BOOL* -- true if restore was successful

        """
        debug.trace('SB_ServiceFullState.restore_state', 'cookie=%s' % cookie)
        debug.trace('SB_ServiceFullState.restore_state',
            'buffer %s' % self.buff.name())
        if not self.valid_cookie(cookie):
            return 0
        if debug.trace_is_active('SB_ServiceFullState.restore_state'):
            contents = cookie.contents()
            first_lines = re.match(r'.*\n.*\n', contents).group()
            last_lines = re.search(r'\n.*\n.*\n?$', contents).group()
                    
        self.buff.set_text(cookie.contents())
        
        
        self.buff.set_selection(cookie.get_selection(), cursor_at =
            cookie.cursor_at())
        self.buff.print_buff_if_necessary()
        self.buff.last_search = cookie.last_search()
        return 1
      
    def compare_states(self, first_cookie, second_cookie, selection = 0):
        """compares the buffer states at the times when
        two cookies were returned by store_current_state.  By default,
        only the buffer contents are compared, not the selection, unless
        selection == 1.  If the state corresponding to either cookie has
        been lost, compare_states will return false.

        **INPUTS**

        *SourceBuffCookie* first_cookie, second_cookie -- see 
        store_current_state.  Note that SourceBuffCookie is a dummy 
        type, not an actual class.  The actual type will vary with 
        SourceBuff subclass.

        *BOOL* selection -- compare selection as well as contents

        **OUTPUTS**

        *BOOL* -- true if states are the same, false if they are not, or
        it cannot be determined due to expiration of either cookie
        """
# if unable to make comparison, treat as false
        if not self.valid_cookie(first_cookie):
            return 0
        if not self.valid_cookie(second_cookie):
            return 0

        if first_cookie.contents() != second_cookie.contents():
            return 0
        if not selection:
            return 1
        return self.compare_state_selections(first_cookie, second_cookie)

    def compare_state_selections(self, first_cookie, second_cookie):
        """compares the selection and cursor positions at the times when
        two cookies were returned by store_current_state.
        If the state corresponding to either cookie has
        been lost, compare_states will return false.

        This method does not synchronize with the editor prior to
        comparing with "current".  To ensure that the "current" state 
        is really current, the caller must synchronize.
        (This avoids having duplicate synchronize calls 
        when comparing with the current state of more than one buffer).

        **INPUTS**

        *SourceBuffCookie* first_cookie, second_cookie -- see 
        store_current_state.  Note that SourceBuffCookie is a dummy 
        type, not an actual class.  The actual type will vary with 
        SourceBuff subclass.

        **OUTPUTS**

        *BOOL* -- true if position and selection are the same, false if 
        they are not, or it cannot be determined due to expiration of 
        either cookie
        """
# if unable to make comparison, treat as false
        if not self.valid_cookie(first_cookie):
            return 0
        if not self.valid_cookie(second_cookie):
            return 0
        return first_cookie.get_selection() == second_cookie.get_selection() \
            and first_cookie.cursor_at() == second_cookie.cursor_at()


    def compare_with_current(self, cookie, selection = 0):
        """compares the current buffer state to its state at the time when
        the cookie was returned by store_current_state.  By default,
        only the buffer contents are compared, not the selection, unless
        selection == 1.  If the state corresponding to the cookie has
        been lost, compare_with_current will return false.

        **INPUTS**

        *SourceBuffState cookie* -- see store_current_state.

        *BOOL* selection -- compare selection as well as contents

        **OUTPUTS**

        *BOOL* -- true if state is the same, false if it is not, or
        it cannot be determined due to expiration of the cookie
        """
        if not self.valid_cookie(cookie):
            return 0
# unable to make comparison, so treat as false
        if self.buff.contents() != cookie.contents():
            return 0
        if not selection:
            return 1
        cookie_sel = cookie.get_selection()
        cookie_pos = cookie_sel[cookie.cursor_at()]
        return self.compare_selection_with_current(cookie)
        
    def compare_selection_with_current(self, cookie):
        """compares the current buffer position and selection to these
        values at the time when the cookie was returned by 
        store_current_state.  If the state corresponding to the cookie has
        been lost, compare_with_current will return false.

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        **OUTPUTS**

        *BOOL* -- true if position and selection are the same, false if 
        they are not, or if it cannot be determined due to 
        expiration of the cookie
        """
        if not self.valid_cookie(cookie):
            return 0
# unable to make comparison, so treat as false
        cookie_sel = cookie.get_selection()
        cookie_pos = cookie_sel[cookie.cursor_at()]
        return self.buff.get_selection() == cookie_sel \
            and self.buff.cur_pos() == cookie_pos

    def get_state_pos_selection(self, cookie):
        """retrieves the position and selection from a given state
        cookie.  

        **INPUTS**

        *SourceBuffCookie cookie* -- see store_current_state.  Note that
        SourceBuffCookie is a dummy type, not an actual class.  The
        actual type will vary with SourceBuff subclass.

        **OUTPUTS**

        *(INT, (INT, INT))* -- position and selection at the time the
        cookie was created by store_current_state, or None if the cookie
        is invalid (usually because the state corresponding to the cookie 
        has been lost).
        """
        if not self.valid_cookie(cookie):
            return None
        cookie_sel = cookie.get_selection()
        cookie_pos = cookie_sel[cookie.cursor_at()]
        return (cookie_pos, cookie_sel)

    def valid_cookie(self, cookie):
        """checks whether a state cookie is valid or expired.
        If the state corresponding to the cookie has
        been lost, valid_cookie will return false.

        **INPUTS**

        *SourceBuffState cookie* -- see store_current_state. 

        **OUTPUTS**

        *BOOL* -- true if cookie is valid (i.e. restore_state should be
        able to work)
        """
# this is not intended to be a complete test.  Basically, valid_cookie
# is more important for SourceBuffs which have an internal undo-stack or
# change history.  In the case, however, given the brute force implementation of
# SourceBuffState, there isn't really much point in trying to detect whether 
# the caller has forged a cookie.

# do make sure that it is at least a subclass of SourceBuffState,
# otherwise other cookie-related SourceBuffTB methods will fail
        if not issubclass(cookie.__class__, self._state_cookie_class()):
            return 0

        return self.buff.name() == cookie.name()

