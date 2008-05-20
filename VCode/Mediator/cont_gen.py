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

"""Context objects which are not tied to a specific language"""
try:
    set
except NameError:
    from sets import Set as set

from Context import Context
import actions_gen 
from debug import trace
import AppStateEmacs
import re
from vc_globals import *
##from utilsqh import curry

class ContLanguage(Context):
    """Context that applies only if a particular programming language is the
    active one.
    
    **INSTANCE ATTRIBUTES**
    
    *ANY language=None* -- Name of the programming language for this context. If *None*, then this context always applies.

    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, language, **args_super):

        # Ensure language is always a tuple of one or more valid languages
        if language == None:
            language = all_languages
        elif isinstance(language, basestring):
            language = (language, ) 
        elif isinstance(language, tuple):
            pass
        else:
            raise TypeError("Language of ContLanguage instance must be a string or tuple, not: %s"% \
                            repr(language))
        list_of_valid_languages = []
        for lang in language:
            if lang not in max_all_languages:
                raise(ValueError, 'ContLanguage is called with invalid language: "%s"\n'
                                  'valid languages are: %s'% (lang, repr(all_languages)))
            if lang in all_languages:
                # only take languages that currently defined
                list_of_valid_languages.append(lang)
        list_of_valid_languages.sort()  # maybe empty...
        self.language = tuple(list_of_valid_languages)
        super(ContLanguage, self).__init__(**args_super)

    def scope(self):
        """returns a string indicating the scope of this context.
        Commands with more specific scopes are checked first.

        See Context for details of the recognized scopes


        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the string identifying the scope
        """
        return "buffer"

    def equivalence_key(self, prefix="Language"):
        """returns a key used to separate Context instances into
        equivalence classes.  Two contexts which are equivalent (i.e.
        share the same set of circumstances under which they apply)
        should have identical keys.  Two contexts which are not
        equivalent should have distinct keys.

        For example, two instances of ContPy should both return the same
        key.

        See Context for more details.

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the key
        """
        if self.language == all_languages:
            lang = "any"
        else:
            lang = "|".join(self.language)
        return "%s: %s" % (prefix, lang)

    def _applies(self, app, preceding_symbol = 0):
        buff = app.curr_buffer()
        if buff == None:
            trace("ContLanguage.applies", "buff == None")
            return False
        
        return buff.language_name() in self.language

    def overlaps_with(self, other_context):
        """Check if this context overlaps with another
         
        for language instances and its children, check for overlapping languages
          in QH words: other_context is same class as self, or a subclass of self

        """
        language1 = set(self.language)
        language2 = set(other_context.language)
        # see if intersection of 2 sets is not empty:

        if language1 & language2:
            trace('ContLanguage.overlaps_with', 'overlapping, language1: %s, language2: %s' % \
                    (language1, language2))
            return True
        else:
            trace('ContLanguage.overlaps_with', 'NOT overlapping, language1: %s, language2: %s' % \
                    (language1, language2))

       
# do NOT do this any more, use instances directly:
class ContC(ContLanguage):
    def __init__(self, **attr):
        raise DeprecationError('deprecated class "ContC", use instance "contC" or "ContLanguage(\'C\')" instead')

class ContPerl(ContLanguage):
    def __init__(self, **attr):
        raise DeprecationError('deprecated class "ContPerl", use instance "contPerl" or "ContLanguage(\'perl\')" instead')

class ContPy(ContLanguage):
    def __init__(self, **attr):
        raise DeprecationError('deprecated class "ContPy", use instance "contPy" or "ContLanguage(\'python\')" instead')
    
class ContAny(Context):
    """This context always applies, UNLESS translation is off."""

    def __init__(self, **attrs):
        super(ContAny, self).__init__(attrs)
        
    def _applies(self, app, preceding_symbol = 0):
        return not app.translation_is_off

    def scope(self):
        """returns a string indicating the scope of this context.
        Commands with more specific scopes are checked first.

        See Context for details of the recognized scopes

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the string identifying the scope
        """
        return "global"

    def equivalence_key(self):
        """returns a key used to separate Context instances into
        equivalence classes.  Two contexts which are equivalent (i.e.
        share the same set of circumstances under which they apply)
        should have identical keys.  Two contexts which are not
        equivalent should have distinct keys.

        For example, two instances of ContPy should both return the same
        key.

        See Context for more details.

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the key
        """
        return "Any"


class ContLastActionWas(Context):
    """This context applies if the last action application's command history
    was of a certain type"""

    def __init__(self, types, connector='and', **attrs):
        """**INPUTS**

        *CLASS* types -- A list of class objects (not instance). The
        context applies if the last action is an instance of all them
        (or *one of them* if *self.connector == 'or'*).

        *STR* connector='and' -- If *'and'*, then context applies if
         last action is an instance of all the classes in *types*. If
         *'or'*, then context applies if last action is an instance of
         any of the classes in *types*.
        """
        self.types = types
        if connector == 'and':
            self.connector = connector
        else:
            self.connector = 'or'
        super(ContLastActionWas, self).__init__(attrs)
        
    def _applies(self, app, preceding_symbol = 0):
        if preceding_symbol:
            last_cont = None
            last_action = actions_gen.ActionInsert('%dummy%')
        else:
            entry = app.get_history(1)
            trace('ContLastActionWas.applies', 'entry=%s' % repr(entry))
            if entry:
                (last_cont, last_action) = entry
            else:
                return 0
        if self.connector == 'and':
            answer = 1
            for a_class in self.types:
                if not isinstance(last_action, a_class):
                    answer = 0
                    break
        else:
            answer = 0
            for a_class in self.types:
                if isinstance(last_action, a_class):
                    answer = 1
                    break
        trace('ContLastActionWas.applies', 'last_cont=%s, last_action=%s, self.types=%s, answer=%s' % (last_cont, last_action, self.types, answer))
        return answer

    def scope(self):
        """returns a string indicating the scope of this context.
        Commands with more specific scopes are checked first.

        See Context for details of the recognized scopes

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the string identifying the scope
        """
        return "last command"

    def equivalence_key(self):
        """returns a key used to separate Context instances into
        equivalence classes.  Two contexts which are equivalent (i.e.
        share the same set of circumstances under which they apply)
        should have identical keys.  Two contexts which are not
        equivalent should have distinct keys.

        For example, two instances of ContPy should both return the same
        key.

        See Context for more details.

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the key
        """
        type_names = []
        for t in self.types:
            type_names.append(str(t))
        type_names.sort()
        s = "LastActionWas: %s %s" % (self.connector, type_names)
        return s


class ContBlankLine(ContLanguage):
    """This context applies if the cursor is on a blank line."""
    reBlank = re.compile(r'\s*$')

    def __init__(self, language=None, **attrs):
        super(ContBlankLine, self).__init__(language, **attrs)
       
    def _applies(self, app, preceding_symbol = 0):
       if preceding_symbol:
           return 0
       answer = 0
       if not super(ContBlankLine, self)._applies(app, preceding_symbol):
           return answer

       buff = app.curr_buffer()
       start = buff.beginning_of_line()
       end = buff.end_of_line()
       line = buff.get_text(start, end)
       if self.reBlank.match(line):
          answer = 1
 
       return answer        

    def scope(self):
        """returns a string indicating the scope of this context.
        Commands with more specific scopes are checked first.

        See Context for details of the recognized scopes

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the string identifying the scope
        """
        return "immediate"

    def equivalence_key(self):
        """returns a key used to separate Context instances into
        equivalence classes.  Two contexts which are equivalent (i.e.
        share the same set of circumstances under which they apply)
        should have identical keys.  Two contexts which are not
        equivalent should have distinct keys.

        For example, two instances of ContPy should both return the same
        key.

        See Context for more details.

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the key
        """
        return ContLanguage.equivalence_key(self, prefix="BlankLine")


class ContPyInsideArguments(ContLanguage):
    """This context should apply if the cursor is inside the arguments section


    For python, inside a function definition or a function call.  So between the parens.

    def f(here):
        pass

    or 

    x = f(here)

    This context can be used for formatting the "=" without spacing when inside. 
    (QH, dec 2006)

    """   
    re_function_call = re.compile(r'\w\s*[(]')

    def __init__(self, **attrs):
        super(ContPyInsideArguments, self).__init__(language='python', **attrs)

       
    def _applies(self, app, preceding_symbol = 0):
        """return 1 if context applies"""
        answer = 0
        if not super(ContPyInsideArguments, self)._applies(app, preceding_symbol):
            return answer

        buff = app.curr_buffer()
        current_pos = buff.cur_pos()
        num = buff.line_num_of(current_pos)
        start = buff.beginning_of_line()
        lineleft = buff.get_text(start, current_pos)
        buff.goto(current_pos)
        trace('ContPyInsideArguments._applies', 'lineleft: %s'% lineleft)
        has_function_call = self.re_function_call.search(lineleft)
        if has_function_call:
            trace('ContPyInsideArguments._applies', 're_function_call.search OK: %s'% has_function_call)
            return 1
        while num > 0:
            num -= 1
            line = buff.get_text_of_line(num)
            trace('ContPyInsideArguments._applies', 'testing line num %s: "%s"'% \
                  (num, line))
            line = line.strip()
            for ending in ('\\', '"', "'", ','):
                if line.endswith(ending):
                    break
            else:
                # no continuation line, not found:
                return answer
            has_function_call = self.re_function_call.search(line)
            if has_function_call:
                trace('ContPyInsideArguments._applies', 're_function_call.search OK: %s'% has_function_call)
                return 1

        
        trace('ContPyInsideArguments._applies', 'fall through, not apply')
        return answer

    def scope(self):
        """skope of ContPyInsideArguments is immediate

        *STR* -- the string identifying the scope
        """
        return "immediate"

    def equivalence_key(self):
        """returns the equivalence_key of ContPyInsideArguments

        *STR* -- the key
        """
        return ContLanguage.equivalence_key(self, prefix="ContPyInsideArguments")

class ContPyBeforeArguments(ContLanguage):
    """This context should apply if the cursor is at a variable with parens just after


    For python:

    here() applies

    and not_here does not apply

    This context can be used for formatting integrating the with arguments CSC with add arguments 
    (QH, febr 2007

    """
    # matches letters "(" with optional empty space in between:
    re_before_open_paren = re.compile(r'\w*\s*[(]')

    def __init__(self, **attrs):
        super(ContPyBeforeArguments, self).__init__(language='python', **attrs)

       
    def _applies(self, app, preceding_symbol = 0):
        """return 1 if context applies"""
        answer = 0
        # test for language:
        if not super(ContPyBeforeArguments, self)._applies(app, preceding_symbol):
            return answer

        buff = app.curr_buffer()
        current_pos = buff.cur_pos()
        num = buff.line_num_of(current_pos)
        end = buff.end_of_line()
        line_from_here = buff.get_text(current_pos, end)
        just_before_open_paren = self.re_before_open_paren.match(line_from_here) 
        if just_before_open_paren:
            return 1
        return answer

    def scope(self):
        """skope of ContPyBeforeArguments is immediate

        *STR* -- the string identifying the scope
        """
        return "immediate"

    def equivalence_key(self):
        """returns the equivalence_key of ContPyBeforeArguments

        *STR* -- the key
        """
        return ContLanguage.equivalence_key(self, prefix="ContPyBeforeArguments")


class ContTextIsSelected(Context):
    """This context applies if there is some text selected."""

    def __init__(self, **attrs):
        super(ContTextIsSelected, self).__init__(**attrs)
       
    def _applies(self, app, preceding_symbol = 0):
        (start, end) = app.get_selection()
        if start != end:
           return True
        else:
           return False

    def scope(self):
        """returns a string indicating the scope of this context.
        Commands with more specific scopes are checked first.

        See Context for details of the recognized scopes

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the string identifying the scope
        """
        return "immediate"

    def equivalence_key(self):
        """returns a key used to separate Context instances into
        equivalence classes.  Two contexts which are equivalent (i.e.
        share the same set of circumstances under which they apply)
        should have identical keys.  Two contexts which are not
        equivalent should have distinct keys.

        For example, two instances of ContPy should both return the same
        key.

        See Context for more details.

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the key
        """
        return "ContTextIsSelected"



class ContAnyEvenOff(Context):
    """This context always applies, EVEN IF translation is off."""

    def __init__(self, **attrs):
        super(ContAnyEvenOff, self).__init__(**attrs)
        
        
    def _applies(self, app, preceding_symbol = 0):
        return 1

    def scope(self):
        """returns a string indicating the scope of this context.
        Commands with more specific scopes are checked first.

        See Context for details of the recognized scopes

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the string identifying the scope
        """
        return "any"

    def equivalence_key(self):
        """returns a key used to separate Context instances into
        equivalence classes.  Two contexts which are equivalent (i.e.
        share the same set of circumstances under which they apply)
        should have identical keys.  Two contexts which are not
        equivalent should have distinct keys.

        For example, two instances of ContPy should both return the same
        key.

        See Context for more details.

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the key
        """
        return "AnyEvenOff"



class ContTranslationOff(Context):
    """This context only applies when translation of commands is 'off'
    
    **INSTANCE ATTRIBUTES**
    
    *none*-- 
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, **args_super):
        super(ContTranslationOff, self).__init__(**attrs)

    def _applies(self, app, preceding_symbol = 0):
        return app.translation_is_off

    def scope(self):
        """returns a string indicating the scope of this context.
        Commands with more specific scopes are checked first.

        See Context for details of the recognized scopes

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the string identifying the scope
        """
        return "global"

    def equivalence_key(self):
        """returns a key used to separate Context instances into
        equivalence classes.  Two contexts which are equivalent (i.e.
        share the same set of circumstances under which they apply)
        should have identical keys.  Two contexts which are not
        equivalent should have distinct keys.

        For example, two instances of ContPy should both return the same
        key.

        See Context for more details.

        **INPUTS**

        *none*

        **OUTPUTS**

        *STR* -- the key
        """
        return "TranslationOff"


class ContNotAfterNewSymb(ContLanguage):
    """This context applies if the current buffer is in a particular language
    AND the words being interpreted weren't preceded by words which were
    interpreted as being part of a new symbol."""

    def __init__(self, language, **attrs):
        super(ContNotAfterNewSymb, self).__init__(language, **attrs)
        
    
    def _applies(self, app, preceding_symbol = 0):
        if super(ContNotAfterNewSymb, self)._applies(app):
            return not preceding_symbol
    
    def scope(self):
        return "last command"
        
    def equivalence_key(self):
        return super(ContNotAfterNewSymb, self).equivalence_key(prefix="NotAfterSymb")
