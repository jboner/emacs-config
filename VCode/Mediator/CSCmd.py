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

import sys
import copy

from Object import Object
from debug import trace, config_warning
import Context
from cont_gen import ContLanguage

class DuplicateContextKeys(RuntimeError):
    def __init__(self, msg):
        RuntimeError.__init__(self, msg)
        self.msg = msg

class CSCmd(object):
    """Class for Context Sensitive Commands (CSCs).

    A CSC is a phrase which, when uttered into an application, may
    fire a particular action.
    
    A CSC may fire different actions depending on the context of the
    application where it was typed.
        
    **INSTANCE ATTRIBUTES**

    **NOTE:** when CSCs are added to CmdInterp, it stores stores the
    underlying CSCmdDict, rather than the CSC.  Therefore, any
    additional data required by CmdInterp once it concludes that a
    particular CSC applies must be stored in CSCmdDict.
        
    *STR spoken_forms=[]* -- list of alternatives ways that this
     command can be spoken out. 

    *CSCmdDict meanings* -- object which manages the meanings of the
    command

    CLASS ATTRIBUTES**
        
    *none* --

    .. [Context] file:///./Context.Context.html
    .. [Action] file:///./Action.Action.html"""
        
    def __init__(self, spoken_forms=None, meanings=None, docstring=None, 
                 generate_discrete_cmd = 0, **attrs):
        """
        **INPUTS**

        *[STR] spoken_forms* -- list of spoken forms for the command

        *meanings=*{* [Context] *: * [Action] *}* -- Dictionary of
        possible contextual meanings for this command. Key is a context
        and value is an action object to be fired if that context applies.
        
        *BOOL generate_discrete_cmd* -- If true, then generate a discrete command
        for the CSC. Use this for commands like "copy that" whose spoken form
        is already a NatSpeak command, but whose behaviour must be different
        in NatSpeak.

        *STR docstring* -- string documentating the command
        """
        self.spoken_forms = spoken_forms or []
        meanings = meanings or {}
        self.docstring = docstring
        self.meanings = CSCmdList(meanings, generate_discrete_cmd, parent=self)
        for key, val in attrs.items():
            setattr(self, key, val)

    def get_meanings(self):
        """returns the reference of the meanings list...
        """
        return self.meanings



    def applies(self, app, preceding_symbol = 0):
        """test whether any of its contexts applies, and returns

        **INPUTS**

        [AppState] app is the application into which the command was spoken.

        BOOL *preceding_symbol* indicates if a symbol would be inserted
        at the current cursor position before the action corresponding
        to this context was executed.  

        **OUTPUTS**

        *meaning* -- meaning if a Context applies, return the (context,
        action) pair, otherwise None
        """
        return self.meanings.applies(app, preceding_symbol)
        
    def doc(self):
        """Returns the documentation for that CSC.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        return self.docstring or ""

    def replace_spoken(self, spoken_forms):
        """replace the spoken forms of the command 

        **INPUTS**

        *[STR] spoken_forms* -- the new spoken forms

        **OUTPUTS**

        *none*
        """
        self.spoken_forms = spoken_forms[:]

    def add_spoken(self, name, spoken_forms):
        """add the given spoken forms to the command 

        **INPUTS**

        *[STR] spoken_forms* -- the spoken forms to add

        **OUTPUTS**

        *none*
        """
        for spoken in spoken_forms:
            self.spoken_forms.append(spoken)

    def remove_spoken(self, name, spoken_forms):
        """remove the given spoken forms of the command 

        **INPUTS**

        *[STR] spoken_forms* -- the spoken forms to remove

        **OUTPUTS**

        *none*
        """
        new_spoken = []
        for spoken in self.spoken_forms:
            if spoken not in spoken_forms:
                new_spoken.append(spoken)
        self.spoken_forms = new_spoken

    def get_info(self):
        """return the csc and csc_set info of this cscmd instance...

        the resulting dict has keys: doc (of csc), setdescription and setname (of cscset)

        """
        info = dict(doc=self.docstring)
        try:
            description = self.parent.description
        except (KeyError, AttributeError):
            description = "no description"
        try:
            name = self.parent.name
        except (KeyError, AttributeError):
            name = 'cscs'
        info['setdescription']  = description
        info['setname']  = name
        return info

class CSCmdList(list):
    """underlying class used to store CSCmd context and action data 
    internally for CSCmd and also once the commands have been indexed 
    within CmdInterp.
    
    **INSTANCE ATTRIBUTES**

    self (the list) will be populated by (context, action) tuples, which are sorted
    at define time by scope, classname and equivalence key
    
    *BOOL generate_discrete_cmd=0* -- If true, then a command should be 
    added to the discrete commands grammar for CSCs.

    CLASS ATTRIBUTES**
        
    *none* --

    .. [Context] file:///./Context.Context.html
    .. [Action] file:///./Action.Action.html"""
        
    def __init__(self, meanings=None, generate_discrete_cmd=0, parent=None, **attrs):
        """

        """
        self.generate_discrete_cmd = generate_discrete_cmd
        self.parent = parent
        if meanings == None:
            pass
        else:
            self._add_meanings(meanings)

    def clone(self):
        """returns a mixed deep-shallow copy of the object.  
        **OUTPUTS**

        *CSCmdList* -- the copy
        """
        return copy.copy(self)    

    def _extract_meanings(self):
        """private method which converts the actions and contexts from a
        CSCmdDict back into the meanings (Dict) argument expected by the
        constructor and by _add_meanings, trivial now list is used

        **INPUTS**

        *none*

        **OUTPUTS**

        *meanings=*{* [Context] *: * [Action] *}* -- Dictionary of
        possible contextual meanings for this command. Key is a context
        and value is an action object to be fired if that context applies.
        """
        meanings = {}
        for context, action, dummy in self:
            
            meanings[context] = action
        return meanings
    
        
    def _add_meanings(self, meanings):
        """private method which adds new meanings to the dictionary

        **INPUTS**

        *meanings=*{* [Context] *: * [Action] *}* -- Dictionary of
        possible contextual meanings for this command. Key is a context
        and value is an action object to be fired if that context applies.

        **OUTPUTS**

        *none*
        """
        self.duplicates = []
        info = self.parent.get_info()
        for context, action in meanings.items():
            if isinstance(context, (tuple, basestring)):
                # a tuple or string means do a language context on the languages in the tuple:
                context = ContLanguage(context)
            if context:
                self._add_a_meaning(context, action, info)            
            else:
                print 'empty context, skipping'

    def _add_a_meaning(self, context, action, info):
            """add one meaning to the """
            dummy = None
            if not context:
                return
            for prev_context, prev_action, prev_info in self:
                if prev_context.conflicts_with(context):
                    msg = "Trying to define conflicting contexts\n" \
                          "first:  %s (scope: %s, equivalence_key: %s)\n" \
                          "second: %s (scope: %s, equivalence_key: %s)"% \
                          (prev_context.__class__, prev_context.scope(), prev_context.equivalence_key(),
                           context.__class__, context.scope(), context.equivalence_key())
                    raise DuplicateContextKeys(msg)
            # no conflicts, just add to the list, at the desired place:
            for i, (c,a,inf) in enumerate(self):
                if context.scope_number() < c.scope_number():
                    self.insert(i, (context, action, info))
                    break
                if context.scope_number() == c.scope_number():
                    if context.equivalence_key() < c.equivalence_key():
                        self.insert(i, (context, action, info))
                        break
            else:
                self.append((context, action, info))



    def merge(self, cscmd_list):
        """merges another CSCmdList into this one
        """
        for context, action, info in cscmd_list:
            self._add_a_meaning(context, action, info)

    def applies(self, app, preceding_symbol = 0):
        """test whether any of its contexts applies, and returns
        the corresponding meaning (or meanings)

        **INPUTS**

        [AppState] app is the application into which the command was spoken.

        BOOL *preceding_symbol* indicates if a symbol would be inserted
        at the current cursor position before the action corresponding
        to this context was executed.  

        **OUTPUTS**

        *[(Context, Action)]* -- 
        A list of meanings whose contexts apply, or None if no contexts
        apply.  Note: if the list has more than one element, CmdInterp 
        should use the first one, but should print a warning about the 
        ambiguous contexts/meanings for the spoken form
        """
        
        #
        # Try each of the contextual meanings in turn until find one that
        # applies
        #
#        print '-- CSCmd.interpret: self.meanings=%s' % self.meanings
        last_scope = -1
        # make a nearly identical CSCmdList instance, for WhatCanISay purposes, for the CmdInterp
        # only the list of context,action tuples is interesting:
        result = CSCmdList(generate_discrete_cmd=self.generate_discrete_cmd, parent=self.parent)
        for context, action, dummy in self:
##            print 'context: %s, scope: %s'% (context, context.scope_number())
            if result and context.scope_number() > last_scope:
##                print 'result and new scope number (last_scope): %s'% last_scope
                return result
            last_scope = context.scope_number()
            if context.applies(app, preceding_symbol):
                result.append((context, action, dummy))
##                print 'applies, length now: %s'% len(result)
                                
        return result or None
    
    def get_info(self):
        """gives a list of info dicts for testing purposes only.

        For WhatCanISay, you need to use the direct values of each context, action together
        with info dict of CSCmd, which was generated at starting time
        """
        all_info = []
        for c, a, csc_info in self:
            info = copy.copy(csc_info)
            info['scope'] = c.scope()
            info['equiv'] = c.equivalence_key()
            info['action'] = a.doc()
            all_info.append(info)
        return all_info
    