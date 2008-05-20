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

import os, re, string, sys, types
import copy, traceback
import debug

import actions_gen, auto_test, vc_globals
from vc_globals import *
from debug import trace, config_warning, trace_is_active
from actions_C_Cpp import *
from actions_py import *
from AppState import AppState
from CSCmd import CSCmd, DuplicateContextKeys, CSCmdList
from Object import Object, OwnerObject
import SymDict
import symbol_formatting
import sr_interface
import WordTrie
import SpokenUtterance
from SymbolResult import SymbolResult

from SpacingState import *

class DeferInterp(Object):
    """
    abstract base class which allows the interpreter main loop to 
    distinguish phrase translations which should be interpreted 
    immediately from those whose interpretation should be deferred 
    to the new symbol loop
    """
    def __init__(self, **args):
        self.deep_construct(DeferInterp, {}, args)

    def interp_now(self, preceding_symbol = 0):
        """tells the interpreter main loop whether to interpret this 
        object now or whether to append it to the untranslated list
        which will build up the components of a new symbol

        **INPUTS**

        BOOL *preceding_symbol* indicates if there is already
        untranslated text (LSAs generating digits should be interpreted
        immediately if there is no pending text, because digits cannot
        start a symbol name)

        **OUTPUTS**

        *BOOL* -- if true, the object should be interpreted now
        """
        debug.virtual('DeferInterp.interp_now')
    def get_info(self):
        return {'set': "DeferInterp object", 'name': "???", 'setname': "DeferInterp"}

class LSAlias(Object):
    """
    Language-specific alias (or LSA), a word with one or more spoken 
    forms, which is translated into a written form according to the 
    language of the current buffer.
    
    Generally, all combinations of written and spoken forms for an LSA 
    are added to the vocabulary as words, so as to enable
    select-pseudocode.

    **INSTANCE ATTRIBUTES**
    
    *STR* spoken_forms -- List of spoken form of the word.

    *{STR: STR}* meanings -- Dictionary of language specific
     meanings. Key is the language name and value is the written form
     of the LSA for that langugage. The key None is not permitted any more...

    *INT* spacing -- spacing flags, from SpacingState (Note: only a
    handful of these spacing flags are currently used)

    *STR* new_symbol -- flag indicating whether the LSAlias can form
    part of a new symbol.  Recognized values are None if the alias
    is always interpreted on its own (and flushes any pending
    untranslated phrase), 'start' if it can start a new symbol (e.g.
    underscore, letter-alpha), or 'within' if it can appear within a
    symbol but cannot start one (e.g. digits)
    """
    def __init__(self, spoken_forms, meanings, spacing = 0, 
        new_symbol = None, parent=None, name='', **args):
        """
        **INPUTS**

        *STR* spoken_forms -- List of spoken form of the word.

        *{STR: STR}* meanings -- Dictionary of language specific
         meanings. Key is the language name and value is the written form
         of the LSA for that langugage. The key None is not permitted any more.
        also a tuple of languages may be given as key, these are interpreted
        first, before string argument keys are done.  Longest tuples first.

         
        *INT* spacing -- spacing flags, from SpacingState (CURRENTLY
        IGNORED BUT MUST BE SPECIFIED PROPERLY TO INSURE FUTURE
        COMPATABILITY)

        *STR* new_symbol -- flag indicating whether the LSAlias can form
        part of a new symbol.  Recognized values are None if the alias
        is always interpreted on its own (and flushes any pending
        untranslated phrase), 'start' if it can start a new symbol (e.g.
        underscore, letter-alpha), or 'within' if it can appear within a
        symbol but cannot start one (e.g. digits)
        """
        self.deep_construct(LSAlias, 
                            {'spoken_forms': spoken_forms, 
                             'meanings': {}, 
                             'spacing': spacing, 
                             'new_symbol': new_symbol,
                             'parent': parent,
                             'name': name
                            }, 
                            args)
        language_keys = meanings.keys()
        if not language_keys:
            return
        if None in language_keys:
            raise DeprecationError('LSAlias "%s"with language "None" is deprecated,\n'
                                   'use the vc_globals variable  "all_languages" instead'%
                                   spoken_forms)


        # sort keys by tuples first, longest then, in order to be able to
        # make exceptions for for example all_languages
        # {all_languages: 'aaa', c_style_languages: 'bbb', 'C': 'ccc'} will then
        # end up correctly
        decorated = [(type(k)!=types.TupleType, -len(k), k) for k in language_keys]
        decorated.sort()
        language_keys = [k for (d1, d2, k) in decorated]
##        print 'language_keys: %s'% language_keys
        for language in language_keys:
            written_as = meanings[language]
            if isinstance(language, tuple):
                for lang in language:
                    if lang in all_languages:
                        self.meanings[lang] = written_as
                    elif lang not in max_all_languages:
                        raise ValueError('invalid language "%s" in LSA definition (with: "%s"), spoken_forms: %s'%
                                         (lang, repr(language), spoken_forms))
                    else:
                        # if all_languages is restricted, pass silent extraneous definitions
                        pass
            else:
                if language in all_languages:
                    self.meanings[language] = written_as
                elif language not in max_all_languages:
                    raise ValueError('invalid language "%s" in LSA definition with spoken_forms: %s'%
                                     (language, spoken_forms))
                else:
                    # if all_languages is restricted, pass silent extraneous definitions
                    pass
                

class AliasMeaning(DeferInterp, symbol_formatting.SymElement):
    """underlying object used by CmdInterp to store the data associated 
    with an LSAlias meaning

    **INSTANCE ATTRIBUTES**

    *STR* written_form -- the written form of the LSA 

    *INT* spacing -- spacing flags, from SpacingState (CURRENTLY
    IGNORED BUT MUST BE SPECIFIED PROPERLY TO INSURE FUTURE
    COMPATABILITY)

    *STR* new_symbol -- flag indicating whether the LSAlias can form
    part of a new symbol.  Recognized values are None if the alias
    is always interpreted on its own (and flushes any pending
    untranslated phrase), 'start' if it can start a new symbol (e.g.
    underscore, letter-alpha), or 'within' if it can appear within a
    symbol but cannot start one (e.g. digits)
    """
    def __init__(self, written_form, spacing = 0, new_symbol = None, parent=None,
        **args):
        self.deep_construct(AliasMeaning, 
                            {
                             'written_form': written_form, 
                             'spacing_flag': spacing, 
                             'new_symbol': new_symbol,
                             'parent': parent
                            }, 
                            args)
    def written(self):
        """returns the written form of the alias

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *STR* -- the written form
        """
        return self.written_form

    def spacing(self):
        """returns the spacing flag for the alias

        **INPUTS**

        *none*

        **OUTPUTS**
        
        *STR* -- the spacing flag
        """
        return self.spacing_flag

    def interp_now(self, preceding_symbol = 0):
        """tells the interpreter main loop whether to interpret this 
        object now or whether to append it to the untranslated list
        which will build up the components of a new symbol

        **INPUTS**

        BOOL *preceding_symbol* indicates if there is already
        untranslated text (LSAs generating digits should be interpreted
        immediately if there is no pending text, because digits cannot
        start a symbol name)

        **OUTPUTS**

        *BOOL* -- if true, the object should be interpreted now
        """
        if self.new_symbol is None:
            return 1
        if self.new_symbol == 'within' and not preceding_symbol:
            return 1
        return 0

    def make_element(self, spoken):
        """create an AliasElement corresponding to this alias

        **INPUTS**

        *STR spoken* -- the spoken form used to dictate this alias

        **OUTPUTS**

        *AliasElement*
        """
        return AliasElement(self.written(), spoken)

    def get_info(self):
        """get a dict of info for testing and WhatCanISay"""
        info = dict(written_form=self.written(),
                    spacing=self.spacing(),
                    new_symbol=self.new_symbol
                    )
        if self.parent == None:
            info['name'] = "unknown"
        else:
            try:
                info['name'] = self.parent.name
            except KeyError:
                info['name'] =  "not there"
            parent = self.parent
            if parent.parent == None:
                info['description'] = 'no description'
                info['setname'] = 'lsas'
            else:
                try:
                    description = self.parent.parent.description
                except KeyError:
                    description = "no description"
                try:
                    name = self.parent.parent.name
                except KeyError:
                    name = 'lsas'
                description = description or "no description"
                name = name or "lsas"
                info['setdescription']  = description
                info['setname']  = name
                    
        return info
            
class AliasElement(symbol_formatting.SymElement):
    """LSAlias meaning as an element of a symbol
    """

    def __init__(self, written, spoken, **args):
        self.deep_construct(AliasElement, 
                            {
                             'written': written, 
                             'spoken': spoken
                            }, 
                            args)

    def add_to(self, builder):
        """Add alias's written form to the symbol builder

        **INPUTS**

        *SymBuilder builder*

        **OUTPUTS**

        *none*
        """
        match = re.match(r'([a-zA-Z])\.{0,1}$', self.written)
        if match:
            letter = match.group(1)
            spoken = "%s." % string.upper(letter) 
            builder.add_letter(string.lower(letter), spoken)
        else:
            builder.add_word(self.written, self.spoken)


class CapitalizationWord(Object):
    """A word with no written form, but which affects capitalization of
    following word(s) in the symbol
    
    **INSTANCE ATTRIBUTES**
    
    *STR* spoken_forms -- List of spoken form of the word.

    *CapsModifier modifier* -- underlying capitalization data
    """
    def __init__(self, spoken_forms, caps, one_word = 1, **args):
        """
        **INPUTS**

        *STR caps* -- the new capitalization state: 'no-caps', 'normal', 
        'cap', or 'all-caps'

        *BOOL one_word* -- if true, modify capitalization for the next
        word.  If false, modify for all following words until a
        subsequent CapitalizationWord with one_word = 0.  (A subsequent
        CapitalizationWord one_word = 1 will take precedence temporarily)
        """
        self.deep_construct(CapitalizationWord, 
                            {
                             'spoken_forms': spoken_forms, 
                             'modifier': CapsModifier(caps, one_word), 
                            }, 
                            args)

class CapsModifier(DeferInterp):
    """underlying object used by CmdInterp to store the data associated 
    with a CapitalizationWord
    """
    def __init__(self, caps, one_word = 1, **args):
        self.deep_construct(CapsModifier, 
                            {
                             'caps': caps, 
                             'one_word': one_word
                            }, args)

    def interp_now(self, preceding_symbol = 0):
        """tells the interpreter main loop whether to interpret this 
        object now or whether to append it to the untranslated list
        which will build up the components of a new symbol

        **INPUTS**

        BOOL *preceding_symbol* indicates if there is already

        untranslated text (LSAs generating digits should be interpreted
        immediately if there is no pending text, because digits cannot
        start a symbol name)

        **OUTPUTS**

        *BOOL* -- if true, the object should be interpreted now
        """
# manual capitalization is ALWAYS part of new symbol, so it is never
# interpreted immediately (and it couldn't be interpreted safely, 
# since it doesn't implement the rest of the methods of AliasMeaning)
        return 0

    def written(self):
# dummy method to allow CapsModifier to pretend to be an LSAlias
        return ""
    
    def make_element(self, spoken):
        """create a CapsModifierElement corresponding to this modifier
        
        **INPUTS**

        *STR spoken* -- the spoken form used to dictate this modifier

        **OUTPUTS**

        *CapsModifierElement*
        """
        return CapsModifierElement(self.caps, self.one_word)

class CapsModifierElement(symbol_formatting.SymElement):
    """SymElement corresponding to a word which changes capitalization of
    the following word(s)
    """
    def __init__(self, caps, one_word = 1, **args):
        self.deep_construct(CapsModifierElement, 
                            {
                             'caps': caps, 
                             'one_word': one_word
                            }, args)


    def add_to(self, builder):
        """Add element to the symbol builder

        **INPUTS**

        *SymBuilder builder*

        **OUTPUTS**

        *none*
        """
        builder.change_caps(self.caps, one_word = self.one_word)

class CmdSet(Object):
    """a collection of context-sensitive commands which may be deleted,
    renamed, or giving synonyms prior to adding them to the command
    interpreter.

    **INSTANCE ATTRIBUTES**

    *STR* name -- name of the command set (to be used for automatic 
    generation of documentation)

    *STR* description -- description of the command set (to be used 
    for automatic generation of documentation)

    *{STR: CSCmd}* commands -- map from unique command names to
    context-sensitive commands
    """
    def __init__(self, name, description = None, **args):
        """
        **INPUTS**

        *STR* name -- name of the command set (to be used for automatic 
        generation of documentation)

        *STR* description -- description of the command set (to be used 
        for automatic generation of documentation)
        """
        self.deep_construct(CSCmdSet, 
                            {'name': name, 'description': description, 
                             'commands': {}, 'aliases': {}}, args)
    def add_csc(self, command, name = None):
        """add a context-sensitive command to the set

        **INPUTS**

        *CSCmd command* -- the command

        *STR name* -- a unique name for the command, or None to use the
        first item in the spoken form list

        **OUTPUTS**

        *none*
        """
        if not command.meanings:
            print 'no meanings for csc'
            return
        if name is None:
            name = command.spoken_forms[0]
        for c,a,info in command.meanings:
            # insert csc_set info in all meanings items:
            info['setname'] = self.name
            info['setdescription'] = self.description
        trace('CmdInterp.add_csc', "add csc with info:  %s"% command.meanings[0][-1])
        self.commands[name] = command

    def add_lsa(self, alias, name = None):
        """add a language-specific alias to the set

        **INPUTS**

        *LSAlias alias* -- the alias

        *STR name* -- a unique name for the alias, or None to use the
        first item in the spoken form list

        **OUTPUTS**

        *none*
        """
        if not alias.meanings:
##            print 'skip adding empty lsa'
            return
        if name is None:
            name = alias.spoken_forms[0]
        alias.parent = self
        self.aliases[name] = alias

CSCmdSet = CmdSet
LSAliasSet = CmdSet

##class CSCmdSet(Object):
##    """a collection of context-sensitive commands which may be deleted,
##    renamed, or giving synonyms prior to adding them to the command
##    interpreter.
##
##    **INSTANCE ATTRIBUTES**
##
##    *STR* name -- name of the command set (to be used for automatic 
##    generation of documentation)
##
##    *STR* description -- description of the command set (to be used 
##    for automatic generation of documentation)
##
##    *{STR: CSCmd}* commands -- map from unique command names to
##    context-sensitive commands
##    """
##    def __init__(self, name, description = None, **args):
##        """
##        **INPUTS**
##
##        *STR* name -- name of the command set (to be used for automatic 
##        generation of documentation)
##
##        *STR* description -- description of the command set (to be used 
##        for automatic generation of documentation)
##        """
##        self.deep_construct(CSCmdSet, 
##                            {'name': name, 'description': description, 
##                             'commands': {}}, args)
##    def add_csc(self, command, name = None):
##        """add a context-sensitive command to the set
##
##        **INPUTS**
##
##        *CSCmd command* -- the command
##
##        *STR name* -- a unique name for the command, or None to use the
##        first item in the spoken form list
##
##        **OUTPUTS**
##
##        *none*
##        """
##        if not command.meanings:
##            print 'no meanings for csc'
##            return
##        if name is None:
##            name = command.spoken_forms[0]
##        for c,a,info in command.meanings:
##            # insert csc_set info in all meanings items:
##            info['setname'] = self.name
##            info['setdescription'] = self.description
##        trace('CmdInterp.add_csc', "add csc with info:  %s"% command.meanings[0][-1])
##        self.commands[name] = command
##
##    def replace_spoken(self, name, spoken_forms):
##        """replace the spoken forms of a command with the given name
##
##        **INPUTS**
##
##        *STR name* -- unique name of the command given when it was added
##
##        *[STR] spoken_forms* -- the new spoken forms
##
##        **OUTPUTS**
##
##        *BOOL* -- true if a command by that name existed
##        """
##        try:
##            command = self.commands[name]
##            command.replace_spoken(spoken_forms)
##        except KeyError:
##            return 0
##        return 1
##
##    def add_spoken(self, name, spoken_forms):
##        """add the given spoken forms to a command with the given name
##
##        **INPUTS**
##
##        *STR name* -- unique name of the command given when it was added
##
##        *[STR] spoken_forms* -- the spoken forms to add
##
##        **OUTPUTS**
##
##        *BOOL* -- true if a command by that name existed
##        """
##        try:
##            command = self.commands[name]
##            command.add_spoken(spoken_forms)
##        except KeyError:
##            return 0
##        return 1
##
##
##    def remove_spoken(self, name, spoken_forms):
##        """remove the given spoken forms of a command with the given name
##
##        **INPUTS**
##
##        *STR name* -- unique name of the command given when it was added
##
##        *[STR] spoken_forms* -- the spoken forms to remove
##
##        **OUTPUTS**
##
##        *BOOL* -- true if a command by that name existed
##        """
##        try:
##            command = self.commands[name]
##            command.remove_spoken(spoken_forms)
##        except KeyError:
##            return 0
##        return 1
##
##
##    def remove_command(self, name):
##        """remove a command with the given name
##
##        **INPUTS**
##
##        *STR name* -- unique name of the command given when it was added
##
##        **OUTPUTS**
##
##        *BOOL* -- true if a command by that name existed
##        """
##        try:
##            del self.commands[name]
##        except KeyError:
##            return 0
##        return 1
##
##class LSAliasSet(Object):
##    """a collection of language-specific aliases which may be deleted,
##    renamed, or giving synonyms prior to adding them to the command
##    interpreter.
##
##    **INSTANCE ATTRIBUTES**
##
##    *STR* name -- name of the alias set (to be used for automatic 
##    generation of documentation)
##
##    *STR* description -- description of the alias set (to be used 
##    for automatic generation of documentation)
##
##    *{STR: LSAlias}* aliases -- map from unique names to
##    language-specific aliases
##    """
##    def __init__(self, name, description = None, **args):
##        """
##        **INPUTS**
##
##        *STR* name -- name of the alias set (to be used for automatic 
##        generation of documentation)
##
##        *STR* description -- description of the alias set (to be used 
##        for automatic generation of documentation)
##        """
##        self.deep_construct(LSAliasSet, 
##                            {'name': name, 'description': description, 
##                             'aliases': {}}, args)
##
##    def add_lsa(self, alias, name = None):
##        """add a language-specific alias to the set
##
##        **INPUTS**
##
##        *LSAlias alias* -- the alias
##
##        *STR name* -- a unique name for the alias, or None to use the
##        first item in the spoken form list
##
##        **OUTPUTS**
##
##        *none*
##        """
##        if not alias.meanings:
####            print 'skip adding empty lsa'
##            return
##        if name is None:
##            name = alias.spoken_forms[0]
##        alias.parent = self
##        self.aliases[name] = alias
##
##    def replace_spoken(self, name, spoken_forms):
##        """replace the spoken forms of an alias with the given name
##
##        **INPUTS**
##
##        *STR name* -- unique name of the alias given when it was added
##
##        *[STR] spoken_forms* -- the new spoken forms
##
##        **OUTPUTS**
##
##        *BOOL* -- true if a command by that name existed
##        """
##        try:
##            self.aliases[name].spoken_forms = spoken_forms[:]
##        except KeyError:
##            return 0
##        return 1
##
##    def add_spoken(self, name, spoken_forms):
##        """add the given spoken forms to a command with the given name
##
##        **INPUTS**
##
##        *STR name* -- unique name of the command given when it was added
##
##        *[STR] spoken_forms* -- the spoken forms to add
##
##        **OUTPUTS**
##
##        *BOOL* -- true if an alias by that name existed
##        """
##        try:
##            alias = self.aliases[name]
##        except KeyError:
##            return 0
##        for spoken in spoken_forms:
##            alias.spoken_forms.append(spoken)
##        return 1
##
##    def remove_spoken(self, name, spoken_forms):
##        """remove the given spoken forms of an alias with the given name
##
##        **INPUTS**
##
##        *STR name* -- unique name of the alias given when it was added
##
##        *[STR] spoken_forms* -- the spoken forms to remove
##
##        **OUTPUTS**
##
##        *BOOL* -- true if an alias by that name existed
##        """
##        try:
##            alias = self.aliases[name]
##        except KeyError:
##            return 0
##        new_spoken = []
##        for spoken in alias.spoken_forms:
##            if spoken not in spoken_forms:
##                new_spoken.append(spoken)
##        alias.spoken_forms = new_spoken
##        return 1
##
##
##    def remove_alias(self, name):
##        """remove an alias with the given name
##
##        **INPUTS**
##
##        *STR name* -- unique name of the alias given when it was added
##
##        **OUTPUTS**
##
##        *BOOL* -- true if an alias by that name existed
##        """
##        try:
##            del self.aliases[name]
##        except KeyError:
##            return 0
##        return 1

class CapitalizationWordSet(Object):
    """a collection of CapitalizationWord objects which may be deleted,
    renamed, or giving synonyms prior to adding them to the command
    interpreter.

    **INSTANCE ATTRIBUTES**

    *STR* name -- name of the set (to be used for automatic 
    generation of documentation)

    *STR* description -- description of the set (to be used 
    for automatic generation of documentation)

    *{STR: CapitalizationWord}* words -- map from unique names to
    CapitalizationWord objects
    """
    def __init__(self, name, description = None, **args):
        """
        **INPUTS**

        *STR* name -- name of the set (to be used for automatic 
        generation of documentation)

        *STR* description -- description of the set (to be used 
        for automatic generation of documentation)
        """
        self.deep_construct(CapitalizationWordSet, 
                            {'name': name, 'description': description, 
                             'words': {}}, args)

    def add_capitalization_word(self, word, name = None):
        """add a CapitalizationWord to the set

        **INPUTS**

        *CapitalizationWord word* -- the word

        *STR name* -- a unique name for the word, or None to use the
        first item in the spoken form list

        **OUTPUTS**

        *none*
        """
        if name is None:
            name = word.spoken_forms[0]
        self.words[name] = word

    def replace_spoken(self, name, spoken_forms):
        """replace the spoken forms of a word with the given name

        **INPUTS**

        *STR name* -- unique name of the alias given when it was added

        *[STR] spoken_forms* -- the new spoken forms

        **OUTPUTS**

        *BOOL* -- true if a command by that name existed
        """
        try:
            self.words[name].spoken_forms = spoken_forms[:]
        except KeyError:
            return 0
        return 1

    def add_spoken(self, name, spoken_forms):
        """add the given spoken forms to a command with the given name

        **INPUTS**

        *STR name* -- unique name of the command given when it was added

        *[STR] spoken_forms* -- the spoken forms to add

        **OUTPUTS**

        *BOOL* -- true if a word by that name existed
        """
        try:
            word = self.words[name]
        except KeyError:
            return 0
        for spoken in spoken_forms:
            word.spoken_forms.append(spoken)
        return 1

    def remove_spoken(self, name, spoken_forms):
        """remove the given spoken forms of a word with the given name

        **INPUTS**

        *STR name* -- unique name of the word given when it was added

        *[STR] spoken_forms* -- the spoken forms to remove

        **OUTPUTS**

        *BOOL* -- true if a word by that name existed
        """
        try:
            word = self.words[name]
        except KeyError:
            return 0
        new_spoken = []
        for spoken in word.spoken_forms:
            if spoken not in spoken_forms:
                new_spoken.append(spoken)
        word.spoken_forms = new_spoken
        return 1

    def remove_word(self, name):
        """remove a word with the given name

        **INPUTS**

        *STR name* -- unique name of the word given when it was added

        **OUTPUTS**

        *BOOL* -- true if a word by that name existed
        """
        try:
            del self.words[name]
        except KeyError:
            return 0
        return 1

class SymWord(symbol_formatting.SymElement):
    """a word as an element of a symbol 

    **INSTANCE ATTRIBUTES**

    *STR word* -- the word (possibly abbreviated)

    *STR original* -- original, unabbreviated version (if word is an
    abbreviation/substitution) or None if word is not abbreviated
    """
    def __init__(self, word, original = None, **args):
        self.deep_construct(SymWord, 
                            {
                             'word': word, 
                             'original': original
                            }, 
                            args)

    def add_to(self, builder):
        """Add alias's written form to the symbol builder

        **INPUTS**

        *SymBuilder builder*

        **OUTPUTS**

        *none*
        """
        match = re.match(r'([a-zA-Z])\.{0,1}$', self.word)
        if match:
            letter = match.group(1)
            spoken = "%s." % string.upper(letter) 
            builder.add_letter(string.lower(letter), spoken)
        else:
            builder.add_word(self.word, self.original)

class NoSeparator(symbol_formatting.SymElement):
    """a symbol element which suppresses any separator before the next
    word in the symbol
    """
    def __init__(self, **args):
        self.deep_construct(NoSeparator, {}, args)

    def add_to(self, builder):
        """suppress any separator before the next word in the symbol

        **INPUTS**

        *SymBuilder builder*

        **OUTPUTS**

        *none*
        """
        builder.suppress_separator()

class NoAbbreviation(symbol_formatting.SymElement):
    """a symbol element which suppresses abbreviation of the next
    word in the symbol
    """
    def __init__(self, **args):
        self.deep_construct(NoAbbreviation, {}, args)

    def add_to(self, builder):
        """suppress any abbreviation of the next word in the symbol

        **INPUTS**

        *SymBuilder builder*

        **OUTPUTS**

        *none*
        """
        builder.suppress_abbreviation()

class InterpState(OwnerObject):
    """interface by which CSC actions can modify aspects of the
    interpreter state to affect subsequent commands or the
    interpretation process itself.
    
    examples:
    - Hungarian Notation et al modifies the formatting style of the next
      symbol
    - No-Space will modify the spacing state (once the spacing engine is
      implemented)

    InterpState just provides methods which return other interface
    objects.  The purpose of this architecture is to 
    organize related interpreter state methods, while avoiding the need
    to change the signature of the execute method whenever a new set of
    methods are added

    Note: InterpState retains ownership of all returned objects
    """
    def __init__(self, sym_style, **args):
        """
        **INPUTS**

        *SymStyling sym_style* -- object providing the symbol styling 
        interface
        """
        self.deep_construct(InterpState, 
                            {
                             'sym_style': sym_style
                            }, 
                            args)
        self.add_owned('sym_style')

    def styling_state(self):
        """Returns a reference to an object providing the symbol styling 
        interface

        Note: InterpState retains ownership of the SymStyling object
        """
        return self.sym_style

class SymStyling(OwnerObject):
    """interface to the symbol styling methods of CmdInterp

    """
    def __init__(self, builder_factory, **args):
        self.deep_construct(SymStyling, 
            {'builder_factory': builder_factory}, args)

    def expect(self, identifier):
        """method used to tell the SymBuilderFactory to expect a
        particular type of identifier

        **INPUTS**

        *STR identifier* -- name of the identifier type, or None for a
        generic identifier

        **OUTPUTS**

        *BOOL* -- true if the identifier is known
        """
        return self.builder_factory.expect(identifier)

    def prefer(self, builder):
        """method used to tell the SymBuilderFactory that a CSC has
        given an explicit preference for particular SymBuilder to be
        used for the next symbol

        **INPUTS**

        *STR identifier* -- name of the identifier type, or None for a
        generic identifier

        **OUTPUTS**

        *BOOL* -- true if the identifier is known
        """
        return self.builder_factory.prefer(builder)

    def clear(self):
        """clear expectations and preferences for the next identifier

        **INPUTS**

        *none*

        **OUTPUTS**

        *none*
        """
        return self.builder_factory.clear()
    
class StoredInterpState(Object):
    """
    An object storing the data needed to restore the state of the
    interpreter (e.g. formatting and spacing information).  Note:
    this object is intended to be used only by CmdInterp (as
    contrasted with InterpState which is an interface allowing
    CSC's to manipulate the current state).  It can be retrieved
    and stored by classes outside CmdInterp, using the get_state
    method of CmdInterp, but only in order to pass the same object
    back to the interpret_ methods.
    """
    def __init__(self, formatting_state, **args):
        self.deep_construct(StoredInterpState, 
                            {'formatting_state': formatting_state}, args) 
        
                 
class UtteranceInterpretation(Object):
    """
    Class used to store information about an interpreted phrase and
    the corresponding changes to the buffer, for use by the results
    manager
    """

    def __init__(self, utterance, symbols=[], **args):

        """
        ** INPUTS **
        
        *[SymbolResult] symbols* -- symbols matched or created when
        this phrase was interpreted
        
        *SpokenUtterance utterance* -- utterance that generated this phrase.
        """
        self.deep_construct(UtteranceInterpretation, 
                            {
                             'utterance': utterance, 
                             'symbol_results': symbols
                            }, args)
    def symbols(self):
        """
        returns a list of SymbolResult objects representing the symbols
        found during interpretation

        **INPUTS**

        *none*

        **OUTPUTS**

        *[SymbolResult]* -- The list of symbols
        """
        return self.symbol_results

    def phrase(self):
        """
        returns the list of words in the utterance that was
        interpreted.

        **INPUTS**

        *none*

        **OUTPUTS**

        *[STR]* -- The list of spoken words in the utterance, without
        regard to the original boundaries between words as interpreted
        by the speech engine
        """
        spoken_words = map(lambda x: x[0], self.utterance.words())
        debug.trace('UtteranceInterpretation.phrase', '** returning %s' % spoken_words)
        return spoken_words
        
    def phrase_as_string(self):
        return string.join(self.phrase())

class MockUtteranceInterpretation(UtteranceInterpretation):
    def __init__(self, utterance, symbols, **args):
        apply(UtteranceInterpretation.__init__, [self, utterance, symbols], args)
    
class SymbolConstruction(Object):
    """
    A helper class for CmdInterp which groups together variables
    related to the state of the current symbol being constructed
    
    ** INSTANCE ATTRIBUTES **

    *[STR] untranslated_words* -- the list of untranslated_words
    accumulated so far in the current symbol being built

    *SymBuilderFactory builder_factory* -- a reference to the factory
    for creating new SymBuilder objects

    *SymBuilder builder* -- the current SymBuilder object

    *AppState app* -- the current editor application

    *CmdInterp interp* -- the CmdInterp object which created this
    object

    *[STR] current_preferences* -- the list of current SymBuilder
    preferences at the time when the current symbol was started

    *[SymbolResult] symbols* -- SymbolResult objects corresponding to
    the symbols already created during interpretation of this
    utterance

    *BOOL exact_symbol* -- true if the words which have been added
    so far form an exact symbol match

    *BOOL allow_inexact* -- if true, allow inexact matches to the
    current symbol being built, otherwise allow only exact_matches or
    new symbols
    """
    def __init__(self, interp, app, builder_factory, **args):
        self.deep_construct(SymbolConstruction, 
                            {
                             'untranslated_words': [], 
                             'interp': interp, 
                             'app': app, 
                             'builder_factory': builder_factory, 
                             'builder': None, 
                             'current_preferences': None, 
                             'symbols': [], 
                             'exact_symbol': 0, 
                             'allow_inexact': 1
                            }, args)

        # note: despite the circular reference to CmdInterp, we don't
        # need to make this an OwnerObject, because we don't own
        # anything with a circular reference to us, and because
        # CmdInterp's reference to us is temporary and vanishes at
        # the end of the call to interpret_phrase

    def new_builder(self):
        """
        creates a new SymBuilder object
        """
        self.builder = self.interp.new_builder(self.app)
        self.current_preferences = \
            self.builder_factory.current_preferences(self.app)
   
    def add_symbol(self, spoken_form):
        """
        Adds the spoken form of an exact symbol match to the current
        symbol being built

        ** INPUTS **

        *[STR] spoken_form* -- the spoken_form of the exactly matching
        symbol

        ** OUTPUTS **

        *none*
        """
# if we've already started building a symbol, then we no longer have an
# exact symbol, otherwise we do
        if self.builder:
            self.exact_symbol = 0
        else:
            self.exact_symbol = 1
        self.untranslated_words.extend(spoken_form)
        if self.builder is None:
            self.new_builder()
        for word in spoken_form:
            word_element = self.interp.make_word_element(word)
            word_element.add_to(self.builder)
        debug.trace('SymbolConstruction.add_symbol', 
              'allow_inexact = %d' % self.allow_inexact)
            
    def add_word(self, word):
        """
        Adds a single untranslated word to the current symbol being
        built

        ** INPUTS **

        *STR word* -- the spoken form of the new word
        
        **OUTPUTS**

        *none*
        """
        self.exact_symbol = 0
        self.untranslated_words.append(word)
        if self.builder is None:
            self.new_builder()
        word_element = self.interp.make_word_element(word)
        word_element.add_to(self.builder)
        trace('SymbolConstruction.add_word', 
              'allow_inexact = %d' % self.allow_inexact)

    def add_alias(self, alias, spoken_form):
        """
        Adds a SymElement corresponding to an AliasMeaning or CapsModifier

        ** INPUTS **

        *AliasMeaning or CapsModifier alias* -- the lsa
        
        *STR spoken_form* -- the spoken_form of the alias

        ** OUTPUTS **

        *none*
        """
        self.exact_symbol = 0
        trace('SymbolConstruction.add_alias', 
              'allow_inexact = %d' % self.allow_inexact)
        if self.builder is None:
            self.new_builder()
        written = alias.written()
        if written:
            self.untranslated_words.append(written)
            if not written.isalnum():
                self.allow_inexact = 0
        trace('SymbolConstruction.add_alias', 
              'written = %s, now allow_inexact = %d' % \
              (written, self.allow_inexact))
        element = alias.make_element(spoken_form)
        element.add_to(self.builder)

    def empty(self):
        """
        indicates whether or not there is a symbol under construction
        """
        if not self.builder or self.builder.empty():
            return 1
        return 0

    def reset(self):
        self.untranslated_words = []
        self.builder = None
        self.current_preferences = None
        self.exact_symbol = 0
        self.allow_inexact = 1
        trace('SymbolConstruction.reset', 
              'allow_inexact = %d' % self.allow_inexact)

    def words(self):
        """
        returns a copy of the current list of untranslated words
        """
        return copy.copy(self.untranslated_words)

    def inserted_symbols(self):
        """return the accumulated list of SymbolResult objects
        """
        return self.symbols

    def insert_existing_symbol(self, symbol, found_in_utter, exact_matches = [], 
                        inexact_matches = [], forbidden = []):
        """
        Insert a known symbol with the given written form, track the
        associated changes, and create a SymbolResult
        object

        ** INPUTS **

        *STR symbol* -- the written form of the known symbol
        
        *UtteranceInterpretation found_in_utter* -- the interpreted 
        utterance in which the symbol was found.

        *[STR] exact_matches* -- a prioritized list of exact matches
        to known symbols
      
        *[(INT, STR)] inexact_matches* -- list of (confidence score,
        written form) for known symbols which are possible (but not exact) 
        matches to the spoken form of this symbol, or None if none 
        have been generated yet.

        *[(INT, STR)] forbidden* -- list of (confidence score,
        written form) for known symbols which are forbidden inexact
        matches, or None if none have been generated yet.
        

        ** OUTPUTS **

        *none*
        """
        debug.trace('SymbolConstruction.insert_existing_symbol', 
            'exact_matches=%s, symbol = %s, found_in_utter=%s' % (exact_matches, symbol, found_in_utter))
            
        insertion = actions_gen.ActionInsert(code_bef=symbol, code_after='')
        block, dummy = insertion.log_execute(self.app, None, None)
        result = SymbolResult(symbol, self.words(), 
                              exact_matches, 
                              block, self.app.curr_buffer_name(), 
                              self.current_preferences, 
                              inexact_matches, forbidden, 
                              in_utter_interp = found_in_utter)
        self.symbols.append(result)
        self.reset()
      
    def insert_new_symbol(self, interp_phrase, exact_matches = [], 
                        inexact_matches = [], forbidden = []):
        """
        Generate and insert a new symbol, track the
        associated changes, and create a SymbolResult
        object

        ** INPUTS **
        
        *UtteranceInterpretation* interp_phrase -- The phrase from which
        the symbol was interpreted.

        *[STR] exact_matches* -- a prioritized list of exact matches
        to known symbols
      
        *[(INT, STR)] inexact_matches* -- list of (confidence score,
        written form) for known symbols which are possible (but not exact) 
        matches to the spoken form of this symbol, or None if none 
        have been generated yet.

        *[(INT, STR)] forbidden* -- list of (confidence score,
        written form) for known symbols which are forbidden inexact
        matches, or None if none have been generated yet.
        

        ** OUTPUTS **

        *STR* -- the written form of the new symbol
        """
        debug.trace('SymbolConstruction.insert_new_symbol', 
            'exact_matches=%s, interp_phrase = %s' % (exact_matches, interp_phrase))
        symbol = self.builder.finish()
        insertion = actions_gen.ActionInsert(code_bef=symbol, code_after='')
        block, dummy = insertion.log_execute(self.app, None, None)
        result = SymbolResult(symbol, self.words(), 
                              exact_matches, 
                              block, self.app.curr_buffer_name(), 
                              self.current_preferences, 
                              inexact_matches, forbidden, new_symbol = 1, 
                              in_utter_interp=interp_phrase)
        self.symbols.append(result)
        self.interp.add_symbol(symbol, [self.builder.spoken_form()])
        interp_phrase.symbol_results.append(result)
        self.reset()
        
    
class CmdInterp(OwnerObject):
    """Interprets Context Sensitive Commands spoken into a given application.
    
    **INSTANCE ATTRIBUTES**

    *NewMediatorObject mediator* -- reference to the parent mediator
    which owns this CmdInterp instance

    WordTrie *commands* -- WordTrie mapping spoken form phrases to 
    CSCmdDict objects which contain the data on mappings from Context to
    Action

    [SymDict] *known_symbols* -- dictionary of known symbols
    
    {STR: WordTrie} *language_specific_aliases = {}* -- Key is the name of
     a programming language (None means all languages). Value is a
     WordTrie of AliasMeaning objects over spoken form phrases

    *SymBuilderFactory builder_factory* -- factory for creating new
    SymBuilder objects

    *InterpState state_interface* -- interface passed to
    Action.log_execute, allowing it to affect the interpreter state for
    subsequent commands

    *SymStyling styling_state* -- interface which allows actions 
    affect the formatting style of the next symbol

    BOOL *disable_dlg_select_symbol_matches = None* -- If true, then
    do not prompt the user for confirmation of new symbols.
    
    BOOL *add_sr_entries_for_LSAs_and_CSCs* -- if *TRUE*, then add 
    SR entries for the LSAs and CSCs when they are added. If *FALSE*, 
    assume that these entries were already added by an previous instance
    of the mediator. This is mostly used for regression testing purposes
    where we create a new mediator in each test, and don't want to waste
    CPU time adding the same LSAs and CSCs over and over again.
    
    *STR _spoken_commands_gram_rule=None* -- A natspeak grammar rule that
    matches the spoken form of any LSA or CSC.
    
    *STR _spoken_symbols_gram_rule=None* -- A natspeak grammar rule that
    matches the resolved spoken forms of any known symbol.
    
    CLASS ATTRIBUTES**

    *none* --
        
    .. [AppState] file:///./AppState.AppState.html
    .. [Context] file:///./Context.Context.html
    .. [SymDict] file:///./SymDict.SymDict.html"""
    
    def __init__(self, sym_file = None, 
                 disable_dlg_select_symbol_matches = None, 
                 mediator = None, **attrs):
        
        """
        **INPUTS**

        *STR sym_file = None* -- File used for
        reading/writing the symbol dictionary. If *None*, then don't
        read/write the symbol dictionary from/to file.

        *BOOL disable_dlg_select_symbol_matches = None* -- If true, then
        do not prompt the user for confirmation of new symbols.

        *NewMediatorObject mediator* -- reference to the parent mediator
        which owns this CmdInterp instance
        """

        self.deep_construct(CmdInterp, 
                            {'mediator': mediator, 
                             'commands': WordTrie.WordTrie(), 
                             'known_symbols': None, 
                             'language_specific_aliases': \
                                 {}, 
                             'builder_factory':
                                 symbol_formatting.SymBuilderFactory(), 
                             'state_interface': None, 
                             'disable_dlg_select_symbol_matches': disable_dlg_select_symbol_matches, 
                             'add_sr_entries_for_LSAs_and_CSCs': 1, 
                             '_spoken_commands_gram_rule': None, 
                             '_spoken_symbols_gram_rule': None}, 
                            attrs)
        self.name_parent('mediator')
        self.add_owned('known_symbols')
        self.add_owned('state_interface')
        self.known_symbols = SymDict.SymDict(sym_file = sym_file, interp = self)
        self.styling_state = SymStyling(self.builder_factory)
        self.state_interface = InterpState(self.styling_state)
        # initialise for all valid languages:
        for l in all_languages:
            self.language_specific_aliases[l] = WordTrie.WordTrie()
                
    def supported_languages(self):
        languages = self.language_specific_aliases.keys()
        languages.sort()
        return all_languages
                
    #inserted from what_can_i_say:
    def what_can_I_say(self):
        """Generates HTML index of VoiceCode commands (LSAs and CSCs)
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        print 'caught what can I say'
        try:
            index = self.index_cmds_by_topic()
            self.html_cmd_outline(index)
            self.html_cmds_by_topic(index)
            self.html_cmds_alphabetically(index)
        except:
            print traceback.print_exc()
            print sys.exc_info()[0], sys.exc_info()[1]

##QH is in WhatCanISay I would think:::
##    def index_cmds_by_topic(self):
##        """Creates an index of LSAs and CSC by language and topic.
##        
##        **INPUTS**
##        
##        *none* -- 
##        
##
##        **OUTPUTS**
##        
##        *{STR: {STR: (STR, STR)}* index -- Key is the name of a
##         language and value is a dictionary indexing the LSAs and CSCs
##         for that language by topic. The key of the later dictionary
##         is a topic and the value is a 2ple giving the spoken form of
##         the command and a description of its action.
##        
##        """
##        index = {}
##
##        #
##        # First, index the LSAs
##        #
##        all_languages = self.supported_languages()
##        all_languages.sort()
##        for a_language in all_languages:
##            index[a_language] = []
##            wTrie = self.language_specific_aliases[a_language]
##            for an_LSA in wTrie.items():
##                debug.trace('CmdInterp.index_cmds_by_topic', 
##                            '** an_LSA=%s, len an_LSA: %s' % 
##                            (an_LSA, len(an_LSA)))
##                wordList, entry = an_LSA
##                index[a_language].append((wordList, entry))
##                debug.trace('CmdInterp.index_cmds_by_topic', 
##                            '** written %s:  %s'% (' '.join(wordList), entry.written()))
####
####                for at in dir(entry):
####                    if at == '__doc__' or not at.startswith('__'):
####                        print 'at: %s: %s'% (at, getattr(entry, at))
##               
####                spoken, written = sr_interface.spoken_written_form(an_LSA.voc_entry)
####                print 'spoken: %s, written: %s'% (spoken, written)
####                written = re.sub('\n', '\\n', written)
####                descr = 'insert \'written\''
####                for a_topic in an_LSA.topics:                    
####                    self.html_create_index_entry(a_language, a_topic, spoken, descr)
##
##        #
##        # Then the CSCs
##        #
####        for a_CSC in self.cmd_index:
####            for spoken in a_CSC.spoken_forms:
####                for a_topic in a_CSC.topics:
####                    for a_context, an_action in a_CSC.meanings:
####                        descr = an_action.doc()
####                        try:
####                            a_language = a_context.language
####                        except:
####                            # context is not a language context
####                            a_language = None
####                        if a_language:
####                            self.html_create_index_entry(a_language, a_topic, spoken, descr)
##
##        return index
##            
##
##
##
##
##    def html_cmd_outline(self, index):
##        """Writes HTML code outlining the list of commands.
##        
##        **INPUTS**
##        
##        *{STR: {STR: (STR, STR)}}* index -- Index of commands. See
##        [html_index_cmds_by_topic] for details.
##        
##
##        **OUTPUTS**
##        
##        *none* -- 
##
##        .. [html_index_cmds_by_topic] file:///./CmdInterp.CmdInterp.html#html_index_cmds_by_topic"""
##        
##        outline = """
##<HTML>
##<HEADER>
##<TITLE>VoiceCode: What can I say?</TITLE>
##</HEADER>
##<BODY>
##
##<H1>VoiceCode: What can I say?</H1>
##
##<H2>Index</H2>
##
##<UL>"""
##
##        languages = self.supported_languages()
##        languages.sort()
##        debug.trace('CmdInterp.html_cmd_outline', '** index=%s, languages=%s' % (index, languages))
##        for a_language in languages:
##            
##            if not a_language:
##                a_lang_name = 'Global'
##            else:
##                a_lang_name = a_language
##
##            outline = outline +  "\n" + \
##                      '<LI><A HREF="#%s">%s</A>\n' % (a_lang_name, a_lang_name)
##   
### AD: Not sure what you mean by a topic, so I'm commenting this out for now.
###
###            topics = index[a_language].keys().sort()
###            for a_topic in topics:
###                url = a_lang_name + '-' + a_topic
###                outline = outline + "\n" + \
###                          '      <LI><A HREF="#%s">%s</A>' % (url, a_topic)
###            outline = outline + "\n" + \
###                      '   </UL>'
##
##        outline = outline + "\n" + '</UL>\n<HR>'
##        
##        return outline
##
##
##
##
##    def html_cmds_by_topic(self, index):
##        """Prints HTML index of commands by topic
##        
##        **INPUTS**
##        
##        *{STR: {STR: (STR, STR)}}* index -- See
##        [html_index_cmds_by_topic] for details.
##        
##
##        **OUTPUTS**
##        
##        *none* -- 
##
##        .. [html_index_cmds_by_topic] file:///./CmdInterp.CmdInterp.html#html_index_cmds_by_topic"""
##        
##        html = ""    
##        all_languages = self.supported_languages()
##        all_languages.sort()
##        for a_language in all_languages:        
##            if not a_language:
##                a_lang_name = 'Global'
##            else:
##                a_lang_name = a_language
##
##            html = html + "\n" + \
##                   '<H2><A NAME="%s">%s commands</A></H2>\n\n' % (a_lang_name, a_lang_name)
##
### AD: Not sure what you mean by a topic, so I'm commenting this out for now.        
###            topics = index[a_language].keys().sort()
###            for a_topic in topics:
###                url = a_lang_name + '-' + a_topic
###                print '<H3><A NAME="%s">%s</A></H3>\n\n' % (url, a_topic)
###                for spoken, descr in index[a_language][a_topic]:
###                    print '<STRONG>"%s"</STRONG><BR><DD>%s' % (spoken, descr)
###        
##
##        html = html + "\n" + '</BODY>\n</HTML>'
##        
##        return html
##
##         
##    def html_cmds_alphabetically(self, index):
##        """Does nothing for now"""
##        return ""
                
    def set_mediator(self, mediator):
        """sets the parent mediator which owns this CmdInterp instance

        **INPUTS**

        *NewMediatorObject mediator* -- reference to the parent mediator
        which owns this CmdInterp instance

        **OUTPUTS**

        *none*
        """
        self.mediator = mediator

    def input_error(self, message, fatal = 0):
        """sends a message to NewMediatorObject indicating that a
        serious error occurred while trying to read SymDict information 
        from the persistent dictionary file.  If a GUI is available, the
        message should be displayed in a dialog box before the rest of
        the GUI and server starts.  Otherwise, the message should be
        sent to stderr

        **INPUTS**

        *STR message* -- the message to display

        *BOOL fatal* -- if true, the error is fatal and the mediator
        should clean up and exit the user has confirmed seeing it

        **OUTPUTS**

        *none*
        """
        if self.mediator:
            self.mediator.input_error(message, fatal = fatal)

    def user_message(self, message, instance = None):
        """sends a user message up the chain to the NewMediatorObject to
        be displayed

        **INPUTS**

        *STR message* -- the message

        *STR instance_name* -- the editor from which the message
        originated, or None if it is not associated with a specific
        editor.

        **OUTPUTS**

        *none*
        """
        if self.mediator:
            self.mediator.user_message(message, instance = instance)

    def spoken_form_regexp(self, spoken_form):
        """Returns a regexp that matches a spoken form of a command.

        *STR spoken_form* is the spoken form. The returned regexp will match
        it even if the case of the first letter of each word do not match."""

        words = re.split('\s+', spoken_form)
        regexp = ''
        for aword in words:
            first = aword[0]
            rest = aword[1:]
            regexp_this_word = '[' + string.lower(first) + string.upper(first) + ']' + rest
            if not regexp == '':
                regexp = regexp + '\s*'
            regexp = regexp + regexp_this_word
        return regexp

    def gram_spec_spoken_cmd(self, rule_name, empty=0):
        """returns a NatSpeak grammar rule that matches the spoken form of
        any LSA or CSC.

        **INPUTS**

        *STR rule_name* -- name of the grammar rule

        *BOOL empty* --- If *false*, generate empty grammar.

        **OUTPUTS**

        *STR rule* -- The natspeak rule.
        """
        if self._spoken_commands_gram_rule == None:
           known_spoken_forms = {}
           if not empty:          
              for spoken, cscmdlist in  self.commands.items():
##                  print 'gram_spec_spoken_cmd, spoken: %s, cscmdlist: %s'% (spoken, cscmdlist)
                  if not isinstance(cscmdlist, CSCmdList):
                      raise TypeError("gram_spec_spoken_cmd not valid CSCmdList instance: %s,\n"
                                      "spoken: %s"% (cscmdlist, spoken))
                  generate_discrete_cmd = cscmdlist.generate_discrete_cmd
                  if generate_discrete_cmd:
                      spoken_form = ' '.join(spoken)
                      debug.trace("CmdInterp.gram_spec_spoken_cmd", "** adding %s" % spoken_form)
                      known_spoken_forms[spoken_form] = 1
 
# For now, never add LSAs... later, allow adding discrete commands for LSAs where
# generate_discrete_cmd = 1                 
#              for lsa_word_trie in self.language_specific_aliases.values():                        
#                 for an_lsa_word_trie_entry in lsa_word_trie.items():
#                    spoken_form = string.join(an_lsa_word_trie_entry[0])
#                    known_spoken_forms[spoken_form] = 1
#
           # Note: dummyghjetqwer is an unpronouceable dummy word used to 
           #       make sure this rule will not be empty (which would cause
           #       a crash).           
           self._spoken_commands_gram_rule = "<%s> = dummyghjetqwer" % rule_name
           for a_known_spoken_form in known_spoken_forms.keys():
              self._spoken_commands_gram_rule = self._spoken_commands_gram_rule + "|%s" % a_known_spoken_form
           self._spoken_commands_gram_rule = self._spoken_commands_gram_rule + ";\n"           
        return self._spoken_commands_gram_rule

    def gram_spec_spoken_symbol(self, rule_name, empty=0): 
        """returns a NatSpeak grammar rule that matches the spoken form of
        any known symbol.

        **INPUTS**

        *STR rule_name* -- name of the grammar rule

        *BOOL empty* --- If *false*, generate empty grammar.


        **OUTPUTS**

        *STR rule* -- The natspeak rule.
        """
        if self._spoken_symbols_gram_rule == None:        
           known_spoken_forms = {}
           if not empty:
              for a_word_trie_entry in self.known_symbols.spoken_form_info.items():
                 spoken_form = string.join(a_word_trie_entry[0])
                 known_spoken_forms[spoken_form] = 1           
                      
           # Note: dummyghjetqwer is an unpronouceable dummy word used to 
           #       make sure this rule will not be empty (which would cause
           #       a crash).           
           self._spoken_symbols_gram_rule = "<%s> = dummyghjetqwer" % rule_name                
           for a_spoken_form in known_spoken_forms.keys():
              self._spoken_symbols_gram_rule = self._spoken_symbols_gram_rule + "|%s" % a_spoken_form
        
           self._spoken_symbols_gram_rule = self._spoken_symbols_gram_rule + ";\n"        
           
        return self._spoken_symbols_gram_rule


    def get_state(self):
        """
        Returns an object containing the data needed to restore the
        interpreter to the present state (e.g. spacing and
        formatting_state information).  Note: the StoredInterpState
        object returned should not be modified, and should only be
        used by passing it back to the restore_state

        ** INPUTS **

        *none*

        ** OUTPUTS **

        *StoredInterpState* -- the state information, to be passed
        back to restore_state
        """
        formatting = self.builder_factory.get_state()
        return StoredInterpState(formatting)

    def restore_state(self, state):
        """
        Restores the interpreter to a previously state (including
        spacing and formatting information) obtained from get_state.

        ** INPUTS **

        *StoredInterpState state* -- the state, previously returned by
        get_state

        ** OUTPUTS **

        *BOOL* -- true if the state was successfully restored
        (otherwise it is cleared)
        """
        formatting = state.formatting_state
        # For now, we don't keep track of the SpacingState, so we only
        # need to restore the SymBuilderFactory
        return self.builder_factory.restore_state(formatting)

    def interpret_utterance(self, utterance, app, initial_buffer = None, 
            clear_state = 0):

        """Interprets a natural language command and executes
        corresponding instructions.

        *SpokenUtterance* utterance-- The utterance.

        *AppState app* -- the AppState interface to the editor
        
        *[STR] initial_buffer* -- The name of the target buffer at the 
        start of the utterance.  Some CSCs may change the target buffer of 
        subsequent parts of the command.  If None, then the current buffer 
        will be used.

        *BOOL clear_state* -- if true, clear formatting and spacing
        *states before interpreting the utterance
        """
        interp_phrase = UtteranceInterpretation(utterance)
        phrase_str = utterance.normalized_spoken_phrase()           

        if initial_buffer == None:
            app.bind_to_buffer(app.curr_buffer_name())
        else:
            app.bind_to_buffer(initial_buffer)

        symbols = SymbolConstruction(self, app, self.builder_factory)

        if clear_state:
            self.styling_state.clear()
        
        #
        # Process the beginning of the command until there is nothing
        # left
        #

        while len(phrase_str) > 0:
            trace('CmdInterp.interpret_utterance', 
                'now, phrase_str = %s' % phrase_str)
                
            #
            # Identify leading CSC, LSA, symbol and ordinary word
            #
            possible_CSCs = self.chop_CSC_phrase(phrase_str, app)

            aliases = self.language_specific_aliases
            language = app.active_language()
            chopped_LSA = ""
            LSA_consumes = 0
            if aliases.has_key(language):
                chopped_LSA, LSA_consumes = \
                    self.chop_LSA_phrase(phrase_str, aliases[language])

            chopped_symbol, symbol_consumes, chopped_symbol_is_exact_match = \
                self.chop_symbol_phrase(phrase_str)

            chopped_word = phrase_str[0]
            word_consumes = 1

            most_definite = max((LSA_consumes, symbol_consumes, word_consumes))

            trace('CmdInterp.interpret_utterance', 
            'possible_CSCs=%s, chopped_LSA=%s, LSA_consumes=%s, chopped_symbol=%s, symbol_consumes=%s, chopped_word=%s, word_consumes=%s' % (possible_CSCs, chopped_LSA, LSA_consumes, chopped_symbol, symbol_consumes, chopped_word, word_consumes))
            trace('CmdInterp.interpret_utterance', 
                'most_definite = %d' % most_definite)
            head_was_translated = 0

            #
            # Translate CSC, LSA, symbol or ordinary word at head of command.
            #
            # If more than one translations are possible, choose the one
            # that consumes the most words from the command.
            #
            # In case of ties, use this order of priority: CSC, LSA, symbol,
            # ordinary words. This order goes from most specific to least
            # specific, i.e.
            #
            # - CSCs usually apply in very restricted contexts only
            # - LSAs  apply for a specific language only
            # - Symbols are restricted to sequences of words that are the
            #   spoken form of a known symbol
            # - ordinary words can be anything
            #
            csc_applies = 0

            CSC_consumes = self.apply_CSC(app, possible_CSCs, 
                phrase_str, most_definite, symbols, interp_phrase)

            if CSC_consumes:
                phrase_str = phrase_str[CSC_consumes:]
                head_was_translated = 1
                symbols.reset()

            if not head_was_translated and LSA_consumes == most_definite:
                #
                # LSA consumed the most words from command. Insert it.
                #
                trace('CmdInterp.interpret_utterance', 
                      'processing leading LSA=\'%s\'' % chopped_LSA)
                
                preceding_symbol = not symbols.empty()
                if not chopped_LSA.interp_now(preceding_symbol):
                    spoken_form = phrase_str[:LSA_consumes]
                    symbols.add_alias(chopped_LSA, string.join(spoken_form))
                else:
# flush untranslated words before inserting LSA
                    if preceding_symbol:
                        self.match_untranslated_text(symbols, app, interp_phrase)
                    action = actions_gen.ActionInsert(code_bef = \
                        chopped_LSA.written(), code_after = '')
                    action.log_execute(app, None, self.state_interface)

                phrase_str = phrase_str[LSA_consumes:]
                head_was_translated = 1


            if not head_was_translated and symbol_consumes == most_definite:
                #
                # Symbol consumed the most words from command. Insert it.
                #
                # Note: known symbols are inserted as untranslated
                #       text because often, the user will create new
                #       symbols by prefixing/postfixing existing ones.
                #       For example, if you define a subclass of a known
                #       class SomeClass you may name the new class
                #       SomeprefixSomeClass or SomeClassSomepostfix.
                #
                trace('CmdInterp.interpret_utterance', 
                      'processing leading symbol=\'%s\'' % chopped_symbol)
                symbols.add_symbol(chopped_symbol)
                if not chopped_symbol_is_exact_match:
                    symbols.exact_symbol = False
                    
                phrase_str = phrase_str[symbol_consumes:]
                head_was_translated = 1
                                         
                   
            if not head_was_translated and word_consumes == most_definite:
                #
                # Nothing special translated at begining of command.
                # Just chop off the first word and insert it, marking
                # it as untranslated text.
                #                 
                trace('CmdInterp.interpret_utterance', 
                      'processing leading word=\'%s\'' % chopped_word)
                symbols.add_word(chopped_word)

                phrase_str = phrase_str[word_consumes:]
                head_was_translated = 1

            #
            # Finished translating head of command.
            #
            # Check if it marked the end of some untranslated text
            #
            if (len(phrase_str) == 0) and not symbols.empty():
                #
                # A CSC or LSA was translated, or we reached end of the
                # command, thus marking the end of a sequence of untranslated
                # text. Try to match untranslated text to a known (or new)
                # symbol.
                #
                trace('CmdInterp.interpret_utterance', 
                      'found the end of some untranslated text')
                self.match_untranslated_text(symbols, app, interp_phrase)

            if trace_is_active('CmdInterp.interpret_utterance'):
                untranslated_text = string.join(symbols.words())
                trace('CmdInterp.interpret_utterance', 
                      'End of *while* iteration. untranslated_text=\'%s\', app.curr_buffer().cur_pos=%s' % (untranslated_text, app.curr_buffer().cur_pos()))

        interp_phrase.symbol_results = symbols.inserted_symbols()
        # make sure to unbind the buffer before returning
        app.unbind_from_buffer()

        #
        # Notify external editor of the end of recognition
        #
        app.recog_end()
        return interp_phrase

    def apply_CSC(self, app, possible_CSCs, spoken_list, 
        most_definite, symbols, interp_phrase):
        """check which CSCs apply and execute the greediest one

        **INPUTS**

        *AppState app* -- the target application

        *[STR] spoken_list* -- list of spoken words

        *[(CSCmdDict, INT)] possible_CSCs* -- the possible CSCmds which
        might apply, together with the number of words they consume, in
        order of descending greediness

        *INT most_definite* -- the most words which would be consumed by
        those constructs which definitely apply (i.e. LSAs in the
        current language and known symbols)

        *SymbolConstruction symbols* -- object storing information
        related to any pending untranslated symbols
        
        *InterpretedPhras* interp_phrase -- phrase that the CSC was interpreted
        from.
        
        **OUTPUTS**

        *INT* -- the number of words actually consumed (0 if no CSC
        consuming more than most_definite applies)
        """
        from_utterance = interp_phrase.utterance
        preceding_symbol = not symbols.empty()
        for match in possible_CSCs:
            meanings, CSC_consumes = match
            trace('CmdInterp.apply_CSC', 
                'possible CSC %s, consumes %d' % (meanings, CSC_consumes))
            if CSC_consumes < most_definite:
# LSA or symbol consumes more than the rest of the CSCs, so defer to
# them
                return 0
            applicable = meanings.applies(app, preceding_symbol)
            if not applicable:
                continue
            context, action, ref = applicable[0]
            trace('CmdInterp.apply_CSC', 
                'applicable = %s' % repr(applicable))
            if len(applicable) > 1:
                msg = 'Configuration Warning: phrase %s\n' \
                    % spoken_list[:CSC_consumes]
                msg = msg + \
                    'has more than one applicable context with the same'
                for context, action, ref in applicable:
                    msg = msg + '\ncontext: %s (scope: %s), \n\t\taction: %s' \
                        % (context, context.scope(), action)
                msg = msg + '\nApplying the first context'
                config_warning(msg)
            csc_applies = 1
# flush untranslated words before executing action
            if preceding_symbol:
                self.match_untranslated_text(symbols, app, interp_phrase)
            action.log_execute(app, context, self.state_interface)
            return CSC_consumes
        return 0

    def interpret_NL_cmd(self, cmd, app, initial_buffer = None, 
            clear_state = 0):
        
        """Interprets a natural language command and executes
        corresponding instructions.
        
        NOTE: DO NOT USE THIS FUNCTION IN PRODUCTION CODE. IT'S ONLY THERE
        AS A CONVENIENCE FOR REGRESSION TESTS THAT WANT TO INTERPRET A 
        COMMAND WITHOUT HAVING TO GENERATE AN ACTUAL SPOKEN UTTERANCE.

        *[STR] cmd* -- The command. It is a list of written\spoken words.

        *AppState app* -- the AppState interface to the editor
        
        *[STR] initial_buffer* -- The name of the target buffer at the 
        start of the utterance.  Some CSCs may change the target buffer of 
        subsequent parts of the command.  If None, then the current buffer 
        will be used.
                
        *BOOL clear_state* -- if true, clear formatting and spacing
        *states before interpreting the utterance
        """
        utterance = SpokenUtterance.MockSpokenUtterance(cmd)
        return self.interpret_utterance(utterance, app, initial_buffer = None, 
            clear_state = 0)

    def massage_command(self, command):
        """Massages a command to prepare it for interpretation.

        Makes sure to substitute special characters (e.g. {Spacebar})
        in the written form of words in the command. Also, makes sure
        that the spoken forms are all lowercase, and contain no
        multiple, leading or trailing blanks.
        
        **INPUTS**
        
        *[STR]* command -- The command to be massaged. It's a list of
         written\spoken words.
        
        **OUTPUTS**
        
        *[(STR, STR)]* -- The massaged command
        """
        command_tuples = []
        for a_word in command:
            spoken, written = sr_interface.spoken_written_form(a_word, 
                clean_spoken = 0)
            command_tuples.append((spoken, written))
        return self.massage_command_tuples(command_tuples)

    def match_untranslated_text(self, symbols, app, interp_phrase):
        """Tries to match last sequence of untranslated text to a symbol.
        
        **INPUTS**
        
        *[STR]* untranslated_words -- list of untranslated words

        *AppState* app -- editor into which the command was spoken
        
        *SymbolConstruction symbols* -- object storing information
        related to any pending untranslated symbols
        
        *UtteranceInterpretation interp_phrase -- The phrase that the untranslated
        text is matched from.

        **OUTPUTS**
        
        *none* -- 
        """
        utterance = interp_phrase.utterance
        phrase = map(SpokenUtterance.remove_periods_from_initials, symbols.words())
        untranslated_text = string.join(phrase)
        spoken_form = untranslated_text
        trace('CmdInterp.match_untranslated_text', 
              'untranslated_text="%s"' % untranslated_text)
        trace('CmdInterp.match_untranslated_text', 
              'phrase = %s' % phrase)
        complete_match = self.known_symbols.complete_match(phrase)        
        trace('CmdInterp.match_untranslated_text', 'complete_match=%s' % complete_match)
        if symbols.exact_symbol and \
               not self.builder_factory.manually_specified():
            trace('CmdInterp.match_untranslated_text', 
                'exact symbol spoken "%s"' % (spoken_form))
            trace('CmdInterp.match_untranslated_text', 'complete_match=%s' % complete_match)
            if len(complete_match) > 0:
                written_symbol = self.choose_best_symbol(spoken_form, 
                    complete_match)
                symbols.insert_existing_symbol(written_symbol, interp_phrase, complete_match)
                self.styling_state.clear()
                return
            else:
                msg = "CmdInterp says there was an exact match to a known\n"
                msg = msg + "symbol, but SymDict is not finding any\n"
                msg = msg + "complete match:\n"
                msg = msg + "complete_match is %s\n" % complete_match
                msg = msg + "phrase is %s\n" % phrase
                msg = msg + "match_phrase gives %s\n" \
                    % repr(self.known_symbols.match_head(phrase))
                sys.stderr.write(msg)
        # Match untranslated text to new known symbol or a known symbol with
        # unresolved spoken forms.
        #
        # Don't bother the user if the untranslated text is just the written
        # form of a known symbol or if it's a number
        #
        reg = '[\d\s]+'
        num_match = re.match(reg, untranslated_text)
        if num_match:
            untranslated_text = re.sub('\s', '', untranslated_text)        
            actions_gen.ActionInsert(code_bef=untranslated_text, 
            code_after='').log_execute(app, None, None)
            self.styling_state.clear()
            symbols.reset()
            return

        symbol_matches = None
        inexact_matches = None
        forbidden = None
        trace('CmdInterp.match_untranslated_text', 
              'allow_inexact, manual = %d, %d' % (symbols.allow_inexact, 
              self.builder_factory.manually_specified()))
        if symbols.allow_inexact and \
                not self.builder_factory.manually_specified():
            symbol_matches, weak_matches, forbidden = \
                self.match_pseudo_symbol(untranslated_text)
            trace('CmdInterp.match_untranslated_text', 
                  'symbol_matches=%s' % symbol_matches)
            symbol_matches = self.adjust_match_scores(symbol_matches)
            weak_matches = self.adjust_match_scores(weak_matches)
            forbidden = self.adjust_match_scores(forbidden)
            inexact_matches = symbol_matches[:] + weak_matches

        trace('CmdInterp.match_untranslated_text', 
              'symbol_matches=%s, inexact_matches=%s, forbidden=%s' % (symbol_matches, inexact_matches, forbidden))
        if symbol_matches:
            symbols.insert_existing_symbol(symbol_matches[0][1], interp_phrase, 
                                    exact_matches = complete_match, 
                                    inexact_matches = inexact_matches, 
                                    forbidden = forbidden)
        else:
            symbol = \
                symbols.insert_new_symbol(interp_phrase, exact_matches = complete_match, 
                                          inexact_matches = inexact_matches, 
                                          forbidden = forbidden)
        
        self.styling_state.clear()
        return

    def adjust_match_scores(self, symbol_matches):
        """
        Adjust the scores of inexact matches to take into account
        additional factors (such as the original file and language of
        the known_symbol), and resort the matches

        **INPUTS**

        *[(INT, STR)]* -- list of inexact matches, with confidence
        scores between 0 and 1 and the written_forms of the symbol

        ** OUTPUTS **

        *[(INT, STR)]* -- a similar list, with adjusted scores
        """
        # for now, don't adjust scores
        matches = []
        for score, match in symbol_matches:
            matches.append((score, match))
        matches.sort()
        matches.reverse()
        return matches
        
    def add_symbol(self, symbol, user_supplied_spoken_forms=[], \
                   tentative = 1, add_sr_entries=1):
        """Add a symbol to the dictionary

        **INPUTS**
        
        *STR* symbol -- Symbol to add

        *[STR] user_supplied_spoken_forms* -- Spoken forms for the
         symbol which were supplied explicitly by the user. These
         forms are added even if they are not generated automaticly by
         [get_spoken_forms]. This is useful in cases where the user
         has explicitly supplied spoken forms for a symbol that contains very
         short abbreviations (i.e. abbreviations that are rejected by
         [add_abbreviation]). In such cases, the spoken form wouldn't
         automaticaly be generated by [updated_spoken_forms] and must
         therefore be added explictely by add_symbol.

        *BOOL* tentative = 1 -- If true, symbol is added tentatively,
        and can be removed on undo/correction/reformatting

        *BOOL* add_sr_entries = 1 -- If true, adds symbol to the SR vocabulary.
        
        **OUTPUTS**
        
        *none* -- 
        """
        self.known_symbols.add_symbol(symbol, 
            user_supplied_spoken_forms, tentative = tentative, 
            add_sr_entries = add_sr_entries)

    def correct_symbol(self, spoken_form, bad_written_form, correct_written_form):
        """Correct the written form of a symbol.

        **INPUTS**
        
        *STR* spoken_form -- Spoken form of the symbol.

        *STR bad_written_form* -- written form that was matched incorrectly to
        *spoken_form*.
        
        *STR correct_written_form* -- written form that SHOULLD have been used instead
        for *spoken_form*.
        
        **OUTPUTS**
        
        *none* -- 
        
        **SIDE EFFECTS**
        
        If *bad_written_form* is a new symbol that was created tentatively on account
        of *spoken_form* beind said, then this method will remove *bad_written_form*
        from the dictionnary altogether. Otherwise, it will simply change the 
        priority of *bad_written_form* and *correct_written_form* for
        *spoken_form*.        
        """
        self.known_symbols.correct_symbol(spoken_form, bad_written_form, correct_written_form)


    def current_preferences(self, app):
        """returns the current list of preferences for SymBuilder
        objects
        
        **INPUTS**

        *AppState app* -- the AppState interface to the editor

        **OUTPUTS**

        *[STR]* --  the current preferences
        """
        buff = app.curr_buffer()
        return self.builder_factory.current_preferences(buff)

    def new_builder(self, app):
        """create a new SymBuilder object to generate new
        symbols

        **INPUTS**

        *AppState app* -- the AppState interface to the editor

        **OUTPUTS**

        *SymBuilder* -- the new symbol builder
        """
        buff = app.curr_buffer()
        return self.builder_factory.new_builder(buff)

    def add_identifier(self, identifier, parent = None):
        """defines a new identifier type for the SymBuilderFactory

        *STR identifier* -- name of the new identifier type (must NOT be
        a known identifier type, or a RuntimeError will be raised)

        *STR parent* -- name of the parent (must be a known identifier
        type, or None, or a RuntimeError will be raised)

        **OUTPUTS**
        """
        self.builder_factory.add_identifier(identifier, parent = parent)
 
    def set_builder_preferences(self, builders, identifier = None, 
        language = None):
        """establishes the preferred order for symbol formatting styles
        for a given language and identifier type

        **INPUTS**

        *[STR] builders* -- prioritized list of names of registered SymBuilder
        objects. If one of the builders is unknown, set_preferences raises 
        a RuntimeError.

        *STR identifier* -- name of the identifier to which these
        preference apply, or None to set general preferences for all
        identifiers without their own preferences.  If the identifier type 
        is unknown, set_preferences raises a RuntimeError.

        *STR language* -- name of the language to which these
        preference apply, or None to set general preferences for all
        languages
        """
        self.builder_factory.set_preferences(builders, identifier =
            identifier, language = language)

    def make_word_element(self, word):
        """creates a SymWord symbol element corresponding to a word

        **INPUTS**

        *STR word* -- the original, unabbreviated form of the word

        **OUTPUTS**

        *SymWord* -- the new symbol element
        """
        trace('CmdInterp.make_word_element', 'word = %s' % repr(word))
        abbreviations = self.known_symbols.preferred_abbreviations(word)
        trace('CmdInterp.make_word_element', 'abbreviations = %s' % repr(abbreviations))
        return SymWord(abbreviations[0], original = word)

    def enable_symbol_match_dlg(self, enable = 1):
        """enables or disables the symbol match dialog

        **INPUTS**

        *BOOL* enable -- 1 to enable the dialog, 0 to disable it

        **OUTPUTS**

        *BOOL* -- previous status of the dialog
        """
        current = not self.disable_dlg_select_symbol_matches
        self.disable_dlg_select_symbol_matches = not enable
        return current

    def dlg_select_symbol_match(self, untranslated_text, symbol_matches, app):
        """Asks the user to select a match for pseudo symbol.
        
        **INPUTS**

        *STR* untranslated_text -- untranslated form of the text which
        matched
        
        *[SymbolMatch]* symbol_matches -- List of possible matches.
        

        **OUTPUTS**
        
        *none* -- 

        .. [SymbolMatch] file:///./SymDict.SymbolMatch.html"""
        

        trace('CmdInterp.dlg_select_symbol_match', 'self.disable_dlg_select_symbol_matches=%s' % self.disable_dlg_select_symbol_matches)

        if self.disable_dlg_select_symbol_matches:
            choice_index = 0
        else:
            good_answer = 0
            while not good_answer:
                print 'Associate \'%s\' with symbol (Enter selection):\n' % untranslated_text
                print '  \'0\': no association'
                ii = 1
                for a_match in symbol_matches:
                    sys.stdout.write('  \'%s\': %s' % (ii, a_match.native_symbol))
                    if a_match.is_new:
                        sys.stdout.write(' (*new*)')
                    sys.stdout.write('\n')
                    ii = ii + 1
                sys.stdout.write('\n> ')
                answer = sys.stdin.readline()
                answer_match = re.match('\s*([\d])+\s*', answer)
                if not answer_match:
                    trace('CmdInterp.dlg_select_symbol_match', 'no match')
                else:
                    trace('CmdInterp.dlg_select_symbol_match', 'answer=%s, answer_match=%s, answer_match.groups()=%s' % (answer, answer_match, answer_match.groups()))
                    choice_index = int(answer_match.group(1)) - 1
                    if choice_index < len(symbol_matches) and choice_index >= -1:
                        good_answer = 1
                if not good_answer:
                    print 'Invalid answer \'%s\'' % answer
                    
        #
        # Accept the match
        #
        trace('CmdInterp.dlg_select_symbol_match', 'choice_index=%s' % choice_index)
#        print '-- CmdInterp.dlg_select_symbol_match: choice_index=%s' % choice_index
        if choice_index >= 0:
            #
            # A match was chosen. Accept it and type it instead of the
            # untranslated text.
            #
            chosen_match = symbol_matches[choice_index]
# do this only on correction.  We'll have to add something to add
# symbols tentatively when they've been dictated but not
# corrected, but in that case, we should never add abbreviations (which
# accept_symbol_match does)
#            self.known_symbols.accept_symbol_match(chosen_match)            

            #
            # Insert matched symbol
            #
            actions_gen.ActionInsert(code_bef=chosen_match.native_symbol, 
            code_after='').log_execute(app, None, self.state_interface)            
            if choice_index != 0:
                trace('CmdInterp.dlg_select_symbol_match.mismatch', 
                'selected alternative match %d: %s' % \
                (choice_index, chosen_match.native_symbol))
        else:
            actions_gen.ActionInsert(code_bef=untranslated_text, 
            code_after='').log_execute(app, None, self.state_interface)                        
            if untranslated_text != symbol_matches[0].native_symbol:
                trace('CmdInterp.dlg_select_symbol_match.mismatch', 
                'untranslated text %s != first match %s' % \
                (untranslated_text, symbol_matches[0].native_symbol))
        
    def chop_CSC_phrase(self, phrase, app):
        """Chops the start of a command if it starts with a CSC.
        
        **INPUTS**
        
        *[STR]* phrase -- The list of spoken words in the command, without
        regard to the original boundaries between words as interpreted
        by the speech engine

        **OUTPUTS**

        Returns a list of tuples, each of the form 
        *(meanings, consumed)*, where:
        
        *CSCmdDict meanings* -- the meanings corresponding to the spoken
        form chopped off.

        *INT* consumed* -- Number of words consumed by the CSC from
         the command

        The list is sorted in order of descending numbers of words
        consumed
        """
        matches = self.commands.all_matches(phrase)
        trace('CmdInterp.chop_CSC_phrase', 
            '%d matches' % len(matches))
        match_consumed = []
        for match in matches:
            cmd_dict, rest_spoken = match
            consumed = len(phrase) - len(rest_spoken)
            trace('CmdInterp.chop_CSC_phrase', 
                'words = %d, phrase = %s' % (consumed, 
                phrase[:consumed]))
            match_consumed.append((cmd_dict, consumed))

        return match_consumed
                    
    def chop_LSA_phrase(self, phrase, aliases):
        """Chops off the beginning of a command if it is an LSA.
        
        **INPUTS**
        
        *[STR]* phrase -- The list of spoken words in the command, without
        regard to the original boundaries between words as interpreted
        by the speech engine

        *WordTrie* aliases -- the set of aliases to use for the match

        **OUTPUTS**

        Returns a tuple *(chopped_LSA, consumed)* where:
        
        *AliasMeaning* chopped_LSA -- The AliasMeaning object
        corresponding to the LSA that was chopped off.  If *None*, it 
        means *command* did not start with an LSA.

        *INT* consumed* -- Number of words consumed by the LSA from
         the command
        """
        # See if spoken_form is in the list of active LSAs
        #

        match = aliases.match_head(phrase)
        if match[0] is None:
            return None, 0
        meaning, rest_spoken = match
        consumed = len(phrase) - len(rest_spoken)
        return meaning, consumed
    
    def chop_symbol_phrase(self, phrase):
        """Chops off the beginning of a command if it is a known symbol.
        
        **INPUTS**
        
        *[STR]* phrase -- The list of spoken words in the command, without
        regard to the original boundaries between words as interpreted
        by the speech engine

        **OUTPUTS**

        Returns a tuple *(chopped_symbol, consumed)* where:
        
        *[STR] chopped_symbol* -- spoken form of the known symbol, or None 
        if no symbol matches the spoken form. If *None*, it means 
        *command* did not start with a known symbol.

        *INT* consumed* -- Number of words consumed by the symbol from
         the command
         
         *BOOL* was_exact -- True IIF the consumed words matched the existing symbol
         exactly (i.e. no abbreviations)
        """
        debug.trace('CmdInterp.chop_symbol_phrase', 
            'phrase = %s' % repr(phrase))
        
        
        #
        # Note: We use a higher match_threshold here, to prevent
        #       a known symbol from consuming a trailing LSA or CSC
        #       unless it has good reasons to do so.
        #       For example, if user says "function open paren", 
        #       and there is a known symbol "function", then "function open"
        #       could be a valid match for symbol "function". But 
        #       we want to prevent that. We prevent that by requiring 
        #       strong matches, and "function open" is not a strong match
        #       for symbol "function".
        #
        match = \
         self.known_symbols.match_head(phrase, use_match_threshold=0.6)
        debug.trace('CmdInterp.chop_symbol_phrase', 
            'match = %s' % repr(match))
        if match[0] is None:
            return None, 0, False
        symbols, rest_spoken, exact = match
        consumed = len(phrase) - len(rest_spoken)
        consumed_words = phrase[:consumed]
        was_exact = match[2]
        return consumed_words, consumed, was_exact
                
    def whole_words(self, spoken_list, consumed_words):
        """Checks whether a list of words chopped off the spoken
        list consists of a whole number of words as returned by the
        speech engine
        
        **INPUTS**
        
        *[STR]* spoken_list -- The list of spoken forms in the command

        *[STR]* consumed_words -- The list of words which would be
        consumed 

        **OUTPUTS**

        (*BOOL*, *INT*) -- if the consumed_words consist of a whole
        number N of words from spoken_list, returns a tuple of (1, N).
        Otherwise, returns (0, M) where M is the index of the last
        partial word from spoken_list consumed by consumed_words
        """
        spoken = string.join(spoken_list)
        consumed_phrase = string.join(consumed_words)
        chars_consumed = len(consumed_phrase)
        consumed = 0
        total = -1
#        print 'len(spoken) = %d' % chars_consumed
        for i in range(len(spoken_list)):
#            print total, spoken_list[:i]
            total = total + len(spoken_list[i]) + 1
            if total == chars_consumed:
                consumed = i + 1
                return 1, consumed
            if total > chars_consumed:
                return 0, i
        raise RuntimeError('total length of command less than length consumed!')
        
    def choose_best_symbol(self, spoken_form, choices):
        """Chooses the best match for a spoken form of a symbol.

        For now, we just choose the first item in *choices*, but in
        the future, we might choose the one that appears closest to
        the cursor, or the one that used most recently, or the one
        that best matches the spoken form.
        
        **INPUTS**
        
        *STR* spoken_form -- spoken form of the symbol. 
        
        *[STR]* choices -- list of written forms of symbols having this
        spoken form

        **OUTPUTS**
        
        *none* -- 
        """

        return choices[0]

    def index_csc(self, acmd):
        """Add a new csc to the command interpreter's command dictionary

        [CSCmd] *acmd* is the command to be indexed.

        .. [CSCmd] file:///./CSCmd.CSCmd.html"""

#        debug.trace('CmdInterp.index_csc', 'acmd=%s, acmd.spoken_forms=%s, =%s' % (acmd, acmd.spoken_forms, acmd.meanings))
        debug.trace('CmdInterp.index_csc', 'spoken_forms=%s' % acmd.spoken_forms)
        cmd_list = acmd.get_meanings()
        if not isinstance(cmd_list, CSCmdList):
            raise TypeError('index_csc: cmd_list should be instance of CSCmdList, not: %s\n'
                            '%s'% (type(cmd_list), cmd_list))
        for a_spoken_form in acmd.spoken_forms:
            #
            # Remove leading, trailing and double blanks from the spoken form
            #
            orig_spoken = string.strip(a_spoken_form)
            a_spoken_form = sr_interface.clean_spoken_form(a_spoken_form)

            #
            # Index the spoken form
            #
            phrase = string.split(a_spoken_form)
            meanings = self.commands.complete_match(phrase)
            trace('CmdInterp.index_csc', 'adding phrase %s' % phrase)
            if meanings:
                if not isinstance(meanings, CSCmdList):
                    raise TypeError('index_csc: meanings should be instance of CSCmdList, not: %s\n'
                                    '%s'% (type(meanings), meanings))
                
                try:
                    meanings.merge(cmd_list)
                except DuplicateContextKeys, e:
                    msg = 'Warning: when adding CSC spoken form %s,\n' % phrase
                    msg = msg + e.msg
                    config_warning(msg)

# since meanings is an object reference, we don't need to call
# add_phrase to modify the value corresponding to the
# phrase
            else:
                #
                # First time indexed. Create a new list of CSCs for that
                # spoken form, and add it to the SR vocabulary.
                #
                self.commands.add_phrase(phrase, cmd_list)
                if (self.add_sr_entries_for_LSAs_and_CSCs):
                    sr_interface.addWord(orig_spoken)
# we had some problems in regression testing because the individual
# words in a spoken form were unknown, so now we add the individual
# words in a multiple-word spoken form

# This allows for redundant translation, avoiding
# the problems in regression testing.  However,
# this presumably makes Natspeak recognition of the CSC/LSA worse, 
# so we may want to come up with an alternate solution in the future
                    
                    all_words = string.split(orig_spoken)
                    if (len(all_words) > 1 and 
                        self.add_sr_entries_for_LSAs_and_CSCs):
                        for word in all_words:
                            word = sr_interface.clean_spoken_form(word)
                            sr_interface.addWord(word)

#            print self.commands.all_matches(phrase)

    def add_csc(self, acmd):
        """Add a new Context Sensitive Command. (synonym for index_csc)

        [CSCmd] *acmd* is the command to add.

        .. [CSCmd] file:///./CSCmd.CSCmd.html"""

        self.index_csc(acmd)

    def add_csc_set(self, set):
        """add CSCs from a set

        **INPUTS**

        *CSCmdSet set* -- the set of commands to add

        **OUTPUTS**

        *none*
        """
        debug.trace('CmdInterp.add_csc_set', 
            'adding CSCs from set %s' % set.name)
        for cmd in set.commands.values():
#            print cmd.spoken_forms
            self.add_csc(cmd)

    def add_cmd_set(self, set):
        """integrated add_csc_set and add_lsa_set
        """
        for cmd in set.commands.values():
            self.add_csc(cmd)
        for alias in set.aliases.values():
            self.add_lsa(alias)
            

    def add_lsa(self, an_LSA):
        """Add a language specific word.

        **INPUTS**
        
        *LSAlias an_LSA* -- the new language-specific alias
        
        **OUTPUTS**
        
        *none* -- 
        """

        for language, written_as in an_LSA.meanings.items():
# DCF temporary spacing hack until we put in the real system
            hacked_written_as = written_as
            if an_LSA.spacing & hard_space:
                hacked_written_as = written_as + ' '
            elif an_LSA.spacing & hard_new_line:
                hacked_written_as = written_as + '\n'
            elif an_LSA.spacing & hard_paragraph:
                hacked_written_as = written_as + '\n\n'
            elif an_LSA.spacing & hard_tab:
                hacked_written_as = written_as + '\t'
            elif an_LSA.spacing == like_comma:
                hacked_written_as = written_as + ' '
#                print '%s like comma %d' % (an_LSA.spoken_forms[0], an_LSA.spacing)
            written_as = string.strip(written_as)
# now that we're using hacked_written_as for the LSA entry, there is
# no point in keeping leading and trailing spaces in the written form
# (and besides, they're going to go away as soon as the real spacing
# system is set up)
            if isinstance(language, basestring):
                languages = (language,)
            elif isinstance(language, tuple):
                languages = language
            else:
                raise ValueError('add_lsa, invalid language: "%s" (written form: %s)'% \
                                 (repr(language), written_as))
            for language in languages:
                for spoken_as in an_LSA.spoken_forms:
                    clean_spoken = sr_interface.clean_spoken_form(spoken_as)
                    trace('CmdInterp.add_lsa', 'spoken as: %s, clean_spoken: %s'% (spoken_as, clean_spoken))
                    entry = sr_interface.vocabulary_entry(spoken_as, written_as)
                    vc_entry = sr_interface.vocabulary_entry(spoken_as, written_as, clean_written=0)
    #                if clean_spoken == 'ellipsis':
    #                    print "ellipsis spacing %d, written-as '%s'" \
    #                        % (an_LSA.spacing, written_as)
## do this when initialising the CmdInterp
##                    if not self.language_specific_aliases.has_key(language):
##                        self.language_specific_aliases[language] = \
##                            WordTrie.WordTrie()
                        
                    meaning = AliasMeaning(hacked_written_as, 
                        spacing = an_LSA.spacing, 
                        new_symbol = an_LSA.new_symbol,
                        parent=an_LSA)
                    phrase = string.split(clean_spoken)
                    self.language_specific_aliases[language].add_phrase(phrase, 
                        meaning)
                    trace('CmdInterp.add_lsa', 'language = %s' % repr(language))
                    trace('CmdInterp.add_lsa', 
                        'spoken, written = "%s", "%s"' % (clean_spoken, 
                        written_as))
                    if None in self.language_specific_aliases:
                        self.print_language_specific_aliases()
                        raise ValueError("None should not be a key of an LSAlias %s"% \
                                         self.language_specific_aliases)
                    else:
                        trace('CmdInterp.add_lsa', 'self.language_specific_aliases.keys: %s'%
                              self.language_specific_aliases.keys())
                    #
                    # Add LSA to the SR vocabulary
                    #
                    if self.add_sr_entries_for_LSAs_and_CSCs:
                        trace('CmdInterp.add_lsa', 
                            'adding entry "%s"' % entry)
    #                    print 'clean_spoken, written, entry: "%s", "%s", "%s"' \
    #                        % (clean_spoken, hacked_written_as, entry)
                        sr_interface.addWord(entry)
    # we had some problems in regression testing because the individual
    # words in a spoken form were unknown, so now we add the individual
    # words in a multiple-word spoken form

    # This allows for redundant translation, avoiding
    # the problems in regression testing.  However,
    # this presumably makes Natspeak recognition of the CSC/LSA worse, 
    # so we may want to come up with an alternate solution in the future

                    all_words = string.split(spoken_as)
                    if (len(all_words) > 1 and
                        self.add_sr_entries_for_LSAs_and_CSCs):
                        for word in all_words:
                            word = sr_interface.clean_spoken_form(word)
                            sr_interface.addWord(word)

    def print_language_specific_aliases(self):
        """helper qh"""
        lsa = self.language_specific_aliases
        for lang in lsa:
            wTrie = lsa[lang]
            print 'language: %s'% repr(lang)
            for w,s in wTrie.items():
                print '%s: %s'% (w, s)

    def add_capitalization_word(self, word):
        """Add a language specific word.

        **INPUTS**
        
        *CapitalizationWord word* -- the new word
        
        **OUTPUTS**
        
        *none* -- 
        """
        for spoken_as in word.spoken_forms:
            clean_spoken = sr_interface.clean_spoken_form(spoken_as)
            vc_entry = sr_interface.vocabulary_entry(spoken_as, "", clean_written=0)
            phrase = string.split(clean_spoken)
            for lang in all_languages:
                self.language_specific_aliases[lang].add_phrase(phrase, 
                word.modifier)

            #
            # Add LSA to the SR vocabulary
            #
            if self.add_sr_entries_for_LSAs_and_CSCs:
                trace('CmdInterp.add_capitalization_word', 
                    'adding entry "%s"' % vc_entry)
#                    print 'clean_spoken, written, entry: "%s", "%s", "%s"' \
#                        % (clean_spoken, hacked_written_as, entry)
                sr_interface.addWord(vc_entry)

    def add_lsa_set(self, set):
        """add LSAs from a set

        **INPUTS**

        *LSAliasSet set* -- the set of aliases to add

        **OUTPUTS**

        *none*
        """
        for alias in set.aliases.values():
            self.add_lsa(alias)

    def add_capitalization_word_set(self, set):
        """add CapitalizationWords from a set

        **INPUTS**

        *CapitalizationWordSet set* -- the set of words to add

        **OUTPUTS**

        *none*
        """
        for word in set.words.values():
            self.add_capitalization_word(word)

    def has_lsa(self, spoken_form, language = None):
        """check if there is already an LSA defined with this spoken
        form

        **INPUTS**

        *STR spoken_form* -- spoken form to check

        *STR language* -- name of the language in which to check

        **OUTPUTS**

        *BOOL* -- true if such an LSA exists
        """
        try:
            to_check = self.language_specific_aliases[language]
        except KeyError:
            return 0
        clean_spoken = sr_interface.clean_spoken_form(spoken_form)
        phrase = string.split(clean_spoken)
        if to_check.complete_match(phrase):
            return 1
        return 0

    def add_abbreviation(self, abbreviation, expansions, user_added = 1):
        """Add an abbreviation to VoiceCode's abbreviations dictionary.

        **INPUTS**

        *STR* abbreviation -- the abbreviation 

        *[STR]* expansions -- list of possible expansions


        **OUTPUTS**

        *none* -- 
        """
        self.known_symbols.add_abbreviation(abbreviation, expansions, 
            user_added = user_added)

    def clear_standard_symbols_file_list(self):
        """Clears the list of files defining standard symbols"""
        self.known_symbols.clear_standard_symbols_file_list()

    def standard_symbols_in(self, file_list):
        """Specify source files defining standard symbols"""
        debug.trace('CmdInter.standard_symbols_in', "file_list=%s" % file_list)
        self.known_symbols.standard_symbols_in(file_list)

    def abbreviations_in(self, file_list):
        """Specify source files defining expansions and abbreviations"""
        self.known_symbols.abbreviations_in(file_list)

    def peek_at_unresolved(self):
        """returns a reference to the dictionary of unresolved 
        abbreviations maintained by the SymDict, and the symbols 
        containing those abbreviations.

        **NOTE:** This method is intended only for diagnostic testing
        purpose.  The caller must not modify the dictionary returned

        **INPUTS**

        *none*

        **OUTPUTS**

        *{STR: {STR: 1}}* unresolved_abbreviations={} -- Dictionary of
        unresolved abbreviations. These are abbreviations that have
        appeared in at least one compiled symbol, yet are neither a word
        in the speech vocabulary or a known abbreviation. Values are
        dictionnaries that list the symbols containing the unresolved
        abbreviation.
        """
        return self.known_symbols.peek_at_unresolved()

    def accept_symbol_match(self, the_match):
        """Accepts a match between a pseudo symbol and its native form.

        Adds the new written\spoken symbol to the SR vocabulary and
        adds new abbreviations which are used in the match.

        Also, adds written\spoken symbols for symbols that contain
        those new abbreviations and whose spoken form can now be
        resolved because of those new abbreviations.
        
        **INPUTS**
        
        [SymbolMatch] the_match -- The match to be accepted
        

        **OUTPUTS**
        
        *none* --

        .. [SymbolMatch] file:///./SymDict.SymbolMatch.html"""
        return self.known_symbols.accept_symbol_match(the_match)

    def match_pseudo_symbol(self, pseudo_symbol, use_match_threshold=None):        
        """Returns a prioritized list of all known native symbols that
        match a given pseudo symbol.
        
        **INPUTS**
        
        *STR* pseudo_symbol -- The pseudo symbol to be matched. 
        
        *FLOAT use_match_threshold* -- minimum confidence level required of an
         approximate symbol match. If None, use the default setting of the
         SymDict.


        **OUTPUTS**
        
        *(good_matches, weak_matches, forbidden)* --
        prioritized lists of good, weak (below threshold) and
        forbidden matches, with each match being a 2-tuple of confidence
        score between 0 and 1 (inclusive), and the written form of
        the native symbol
        """
        return self.known_symbols.match_pseudo_symbol(pseudo_symbol)
    
    def remove_symbol_if_tentative(self, symbol):
        """remove a symbol which was tentatively added
        from the dictionary

        **INPUTS**

        *STR* symbol -- native symbol to remove

        **OUTPUTS**

        *BOOL* -- true if the symbol was only tentative, and 
        was successfully removed
        """
        return self.known_symbols.remove_symbol_if_tentative(symbol)
  
    def known_symbol(self, symbol):
        return self.known_symbols.known_symbol(symbol)

    def spoken_forms(self, symbol):
        return self.known_symbols.spoken_forms(symbol)

    def save_dictionary(self, file = None):
        """saves the symbol dictionary state

        **INPUTS**

        *STR file* -- name of the file in which to save the dictionary
        (usually None to use the same file sym_file specified when
        CmdInterp was initialized)

        **OUTPUTS**

        *none*
        """
        self.known_symbols.save(file = file)

    def cleanup_dictionary(self, clean_sr_voc=0, clean_symdict=1, resave=1):
        """Cleans up the symbol dictionary.
        
        **INPUTS**
        
        *BOOL* clean_sr_voc=0 -- If true, then remove symbols from SR
        vocabulary

        *BOOL* clean_symdict=1 -- If true, then removes symbols from
         the symbol dictionary.

        *BOOL resave = 1* -- If true, symbol dictionary is
        resaved to disk after cleanup.        

        **OUTPUTS**
        
        *none* -- 
        """
        self.known_symbols.cleanup_dictionary(clean_sr_voc=clean_sr_voc, 
            clean_symdict=clean_symdict, resave=resave)

    def abbreviations_cleanup(self):
        """Removes all known abbreviations from the symbols dictionary.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        self.known_symbols.abbreviations_cleanup()

    def finish_config(self):
        """Finish performing those parts of the CmdInterp/SymDict
        configuration which can't take place until after the VoiceCode
        configuration files have been executed
        
        **INPUTS**

        *none*
        
        **OUTPUTS**
        
        *none*
        """
# for now, only SymDict requires this
        self.known_symbols.finish_config()
    
    def parse_standard_symbols(self):
        """Parse standard symbols for the various programming languages.
        
        **INPUTS**

        *none*
        
        **OUTPUTS**
        
        *none*
        """
        self.known_symbols.parse_standard_symbols()
    
    def parse_symbols_from_files(self, file_list, add_sr_entries=1):
        """Parse symbols from a series of source files

        **INPUTS**

        *[STR] file_list -- List of files to be compiled

        *BOOL* add_sr_entries = 1 -- If true, add symbols to the SR vocabulary

        **OUTPUT**

        *none* --
        """

        self.known_symbols.parse_symbols_from_files(file_list, 
            add_sr_entries = add_sr_entries)


    def parse_symbols_from_file(self, file_name, add_sr_entries=1):
        """Parse symbols from a single source file.

        *STR* file_name -- The path of the file.

        *BOOL* add_sr_entries = 1 -- If true, add symbols to the SR vocabulary
        """

        self.known_symbols.parse_symbols_from_file(file_name, 
            add_sr_entries = add_sr_entries)
                
    def parse_symbols(self, contents, language_name, add_sr_entries=1):
        """Parse symbols from a string representing the contents of a 
        source file.

        *STR* contents -- the contents of the source file

        *STR* language_name -- the name of the language of the source
        file

        *BOOL* add_sr_entries = 1 -- If true, add symbols to the SR vocabulary
        """
        self.known_symbols.parse_symbols(contents, language_name, 
            add_sr_entries = add_sr_entries)

    def print_symbols(self, symbols = None):
        """Print the content of the symbols dictionary.
        
        **INPUTS**
        
        *[STR] symbols* -- list of symbols to print, or None to print
        the whole dictionary
        
        **OUTPUTS**
        
        *none* -- 
        """
        self.known_symbols.print_symbols(symbols = symbols)

    def print_abbreviations(self, show_unresolved=0):
        """Prints the known and unresolved abbreviations."""
        self.known_symbols.print_abbreviations(show_unresolved)
        
    def generate_training_material(self, num_words=1000000, 
                                   words_per_file=10000, file_names="training"):
        """generate files that can be used to tune NatSpeak's
        language model to "VoiceCodese".
        
        **INPUTS**

        *INT num_words=1 000 000* -- number of words of training
        to be generated.
                                   
        *INT words_per_file=10 000* -- the training material will be split into
        files of that length.
        
        *STR file_names="training"* -- Training files will have that name, with
        an additional numeric suffix (ex: training1, training2).
        
        **OUTPUTS**
        
        *none* -- 
        
        **SIDE EFFECTS**
        
        Files "%VCODE_HOME%/Data/Tmp/training1", etc... are created.
        """
        file_num = 1
        file = None
        while num_words > 0:
           if file:
              file.close()
           file_path = os.path.join(vc_globals.tmp, "%s%s.txt" % 
                                                   (file_names, file_num))
           print "Generating training file: %s" % file_path
           file = open(file_path, "w")
           cmd_spoken_forms = self._cmd_spoken_forms()
           sym_spoken_forms = self._sym_spoken_forms()
           for ii in range(words_per_file/2):
              file.write(" %s" % whrandom.choice(cmd_spoken_forms))
              file.write(" %s" % whrandom.choice(sym_spoken_forms))
              num_words = num_words - 2
           file_num = file_num + 1

    def _cmd_spoken_forms(self):
       known_spoken_forms = {}     
       for cmd_dict in  self.commands.items():
          spoken_form = string.join(cmd_dict[0])
          known_spoken_forms[spoken_form] = 1
                 
       for lsa_word_trie in self.language_specific_aliases.values():
          for an_lsa_word_trie_entry in lsa_word_trie.items():
             spoken_form = string.join(an_lsa_word_trie_entry[0])
             known_spoken_forms[spoken_form] = 1
                  
       return known_spoken_forms.keys()
       
    def _sym_spoken_forms(self):
       known_spoken_forms = {}
       for a_word_trie_entry in self.known_symbols.spoken_form_info.items():
          spoken_form = string.join(a_word_trie_entry[0])
          known_spoken_forms[spoken_form] = 1           
                      
       return known_spoken_forms.keys()
    

    

# defaults for vim - otherwise ignore
# vim:sw=4
