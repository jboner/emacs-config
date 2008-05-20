#############################################################################
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
"""Action functions that span different languages"""
import copy, exceptions, os, re
import Object, vc_globals
import debug
import sr_interface
import symbol_formatting

class Action(Object.Object):
    """Base class for all actions.
    
    **INSTANCE ATTRIBUTES**

    *none* --

    CLASS ATTRIBUTES**
    
    *none* -- 

    """
    
    def __init__(self, docstring=None, **args_super):
        self.deep_construct(Action, \
                            {'docstring': docstring}, \
                            args_super, \
                            {})


    def log_execute(self, app, cont, state = None):
        """Executes the action and logs it in the application's history.
        
        **INPUTS**
        
        [AppState] app -- The application to execute and log the action on.
        
        [Context] cont -- The context object that determines the parameters
        of how the action will be executed.

        [InterpState] state -- interface to the interpreter state

        **OUTPUTS**
        
        *none* -- 

        .. [AppState] file:///./AppState.AppState.html
        .. [Context] file:///./Context.Context.html"""
        
        debug.trace('Action.log_execute', 'invoked, self=%s, cont=%s' % (self, cont))

        #
        # First, make a copies of the action and context before logging them
        # to the command, in case the same prototype context and action
        # instances are being reused everytime a particular CSC is uttered.
        #
        # That way, if we later modify a command in the history (e.g.
        # "previous one" command to reexecute a directional command
        # but in the opposite direction), we won't end up affecting
        # all instances of that action.
        #
        # Note that we don't do this inside of *log* method because some
        # subclasses may override this method and forget to do the copies.
        # For example, actions that just repeat a previous action do not
        # log themselves, but instead log the actions that they repeat.
        #
        action_copy = copy.copy(self)        
        cont_copy = copy.copy(cont)
        debug.trace('Action.log_execute', 'invoking action log()')        
        action_copy.log(app, cont_copy)
        debug.trace('Action.log_execute', 'invoking action execute()')                
        return action_copy.execute(app, cont, state = state)


    def log(self, app, cont):
        """Log the action to an application's command history
        
        **INPUTS**
        
        [AppState] app -- Application to log the action with.

        [Context] cont -- Context in which the action was executed.
        

        **OUTPUTS**
        
        *none* -- 
        """
        debug.trace('Action.log', 'invoked')
        app.log_cmd(cont, self)


    def execute(self, app, cont, state = None):
        """Execute the action.
        
        **INPUTS**
        
        [AppState] app -- Application on which to execute the action
        
        [Context] app_cont -- Context of the application that
        determines the parameters of how the action will be executed.

        [InterpState] state -- interface to the interpreter state
        
                
        **OUTPUTS**
        
        depends on the specific action 

        .. [AppState] file:///./AppState.AppState.html"""
        
        debug.virtual('Action.execute', self)

    def doc(self):
        """Returns a documentation string for the action.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        if getattr(self, 'docstring', None) == None:
            return "no docstring available"
        return self.docstring


class ActionCompound(Action):
    """manually choose the styling for the following symbol

    **INSTANCE ATTRIBUTES**

    *Action action1* -- Action object for the first action to execute
    *Action action2* -- Action object for the second action to execute
    
    """
    
    def __init__(self, action_tuple, **args_super):
        self.deep_construct(ActionCompound, \
                            {'action_tuple': action_tuple}, \
                            args_super, \
                            {})

    def execute(self, app, cont, state = None):
        """Performs the two actions in the ActionCompound

        See [Action.execute] for description of arguments.
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute"""

        debug.trace('ActionCompound.execute', 
                    'Trying to execute all actions.')

        for action in self.action_tuple:
            action.execute(app, cont, state)
            debug.trace('ActionCompound.execute', 
                        '  Executed an action.')

        
        debug.trace('ActionCompound.execute', 
                    'Tried to execute all actions.')


class ActionStyling(Action):
    """manually choose the styling for the following symbol

    **INSTANCE ATTRIBUTES**

    *STR builder* -- name of the SymBuilder instance to use to format
    the next symbol
    """
    
    def __init__(self, builder, **args_super):
        self.deep_construct(ActionStyling, \
                            {'builder': builder}, \
                            args_super, \
                            {})
# want to check this at the time when we are configuring the mediator,
# to prevent errors during recognition
        if not symbol_formatting.registry.known_builder(builder):
            raise RuntimeError('Unknown SymBuilder %s' % builder)

    def execute(self, app, cont, state = None):
        """Execute the action.
        
        **INPUTS**
        
        [AppState] app -- Application on which to execute the action
        
        [Context] app_cont -- Context of the application that
        determines the parameters of how the action will be executed.

        [InterpState] state -- interface to the interpreter state
        
                
        **OUTPUTS**
        
        *none* -- 

        .. [AppState] file:///./AppState.AppState.html"""
        debug.trace('ActionStyling.execute', 'state is %s' % repr(state))
        if state:
            styling = state.styling_state()
            styling.prefer(self.builder)

    def doc(self):
        """Returns a documentation string for the action.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        if getattr(self, 'docstring', None) != None:
            return self.docstring
        else:
            return "Format the next symbol with %s" % self.builder

class ActionRepeatable(Action):
    """Base class for repeatable actions.

    **INSTANCE ATTRIBUTES**
    
    *BOOL already_repeated=0* -- If true, then the action has been repeated at
    least once before.

    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, already_repeated=0, **args_super):
        self.deep_construct(Action,
                            {'already_repeated': already_repeated},
                            args_super,
                            {})

class ActionBidirectional(Action):

    """Base class for bidirectional actions, i.e. actions that can be
    executed in on of two directions (e.g. upward vs downward, left vs
    right, backward vs

    **INSTANCE ATTRIBUTES**
    
    *INT* direction -- 

    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, direction=1, **args_super):
        self.deep_construct(ActionBidirectional,
                            {'direction': direction},
                            args_super,
                            {})


class ActionBidirectionalRepeat(ActionRepeatable, ActionBidirectional):
    """Base class for repeatable bidirectional actions, i.e. bidirectional
    actions that can be repeated and qualified by subsequent utterances
    like: 'previous one', 'next one' or 'reverse'.
   
    **INSTANCE ATTRIBUTES**
    
    *none* --

    CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, **args_super):
        debug.trace('ActionBidirectionalRepeat.__init__', '** invoked')
        self.deep_construct(ActionBidirectionalRepeat,
                            {},
                            args_super,
                            {})
        
class ActionRepeatLastCmd(Action):
    """This action just repeats the last command in the command history.

    **INSTANCE ATTRIBUTES**
    
    *INT n_times=1* -- Number of times to repeate the previous command.

    *BOOL check_already_repeated=0* -- If true, then *n_times* will be
     decreased by 1 before repeating the action, unless the action
     being repeated was already repeated once before.

     This is to allow utterance such as: 'N times' to be used both as a
     qualifier for the command to be repeated, and as a way of repeating
     a command that was already repeated.

     For example, in ['page down', '3 times'], the utterance '3 times' should
     only repeat the 'page down' twice because 'page down' already went down
     one page. But in ['page down', '2 times', '3 times'], the utterance
     should repeat it '3 times' because it was not used as a qualifier for
     'page down'.
    
     **CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, n_times=1, check_already_repeated=0, **args_super):
        self.deep_construct(ActionRepeatLastCmd, 
                            {'n_times': n_times,
                             'check_already_repeated': check_already_repeated},
                           args_super,
                            {})

    def log(self, app, cont):
        """Logs the repeat actions.

        Actually, it just does nothing because instead, the repeated action
        will log itself everytime it is repeated.
        
        See [Action.execute] for description of arguments.
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute"""
        debug.trace('ActionRepeatLastCmd.log', 'invoked')
        pass

    def execute(self, app, cont, state = None):
        """Repeats the last command in the command history [self.n_times].

        See [Action.execute] for description of arguments.
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute
        .. [self.n_times] file:///./actions_gen.ActionRepeatLastCmd.html"""


        (last_cont, last_action) = app.get_history(1)
        debug.trace('ActionRepeatLastCmd.execute', 
                    'last_action=%s, last_action.__dict__=%s, self.check_already_repeated=%s' 
                    % (last_action, last_action.__dict__, self.check_already_repeated))
        if self.check_already_repeated and not last_action.already_repeated:
            self.n_times = self.n_times - 1
            last_action.already_repeated = 1            

        debug.trace('ActionRepeatLastCmd.execute', 'self.n_times=%s' % self.n_times)
        for ii in range(self.n_times):
            last_action.log_execute(app, cont, state = state)


class ActionRepeatBidirectCmd(Action):
    
    """This action repeats the last command only if it was a bidirectional
    action. It may also change the direction of the action.

    **INSTANCE ATTRIBUTES**
    
    *INT direction=1* -- New direction for the action to be
     repeated. If positive, forward/right/down direction. If negative,
     backward/left/up direction. If *None* reverse the direction.
    
    
     **CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, n_times=1, direction=1, **args_super):
        self.deep_construct(ActionRepeatLastCmd, 
                            {'direction': direction}, 
                           args_super,
                            {})

    def log(self, app, cont):
        """Logs the repeat actions.

        Actually, it just does nothing because instead, the repeated action
        will log itself when it is repeated.
        
        See [Action.execute] for description of arguments.
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute"""
        debug.trace('ActionRepeatBidirectCmd.log', 'invoked')
        pass

    def execute(self, app, cont, state = None):
        """Repeats the last command in the command history [self.n_times], if
        it was a bidirectional action. It may also change its direction.

        See [Action.execute] for description of arguments.
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute
        .. [self.n_times] file:///./actions_gen.ActionRepeatLastCmd.html"""

        debug.trace('ActionRepeatBidirectCmd.execute' , 'called, self.__dict__=%s' % self.__dict__)

        #
        # Change the direction of the last action.
        #
        (last_cont, last_action) = app.get_history(1)
        if self.direction == None:
            #
            # Reverse direction of last action
            #
            last_action.direction = last_action.direction * -1
        else:
            last_action.direction = self.direction

        #
        # Repeat the last action
        #
        ActionRepeatLastCmd().log_execute(app, cont, state = state)


class ActionNavigateByPseudoCode(ActionBidirectionalRepeat):
    """This action sets the selection in a buffer to a range specified through a 
    SelectPseudoCode utterance.

    **INSTANCE ATTRIBUTES**

    *[(INT, INT)]* possible_ranges -- Start and end position of the ranges which
    are in the visible portion of the code and which matched the pseudocode specified
    in the SelectPseudoCode utterance.
    
    *INT* select_range_no -- index in *possible_ranges* of the range to be selected.
    

    *INT cursor_at=1* -- If positive, put cursor at end of
     selection. Otherwise, put it at beginning.
     
    *BOOL mark_selection=1* -- If *TRUE*, then select the pseudocode that was uttered. 
    Otherwise, just move the cursor to the start or end of the selection.
    


    *STR buff_name=None* -- Name of file where to set selection. If
     *None*, selectin current buffer.
    
     **CLASS ATTRIBUTES**
    
    *none* -- 
    """
    
    def __init__(self, possible_ranges, select_range_no, mark_selection, cursor_at=1, buff_name=None, 
                       **args_super):
        debug.trace('ActionNavigateByPseudoCode.__init__', 'invoked')
        self.deep_construct(ActionNavigateByPseudoCode,
                            {'possible_ranges': possible_ranges, 
                             'select_range_no': select_range_no, 
                             'cursor_at': cursor_at,
                             'mark_selection': mark_selection,
                             'buff_name': buff_name},
                            args_super,
                            {})
        #
        # We position *select_range_no* one position away from where it should be. That's because
        # when the action gets executed, it will be moved one position in the appropriate 
        # direction.
        #
        self.select_range_no = self.select_range_no - self.direction        

    def execute(self, app, cont, state = None):
        """Selects a region in a buffer.

        See [Action.execute] for description of arguments.
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute
        .. [self.n_times] file:///./actions_gen.ActionRepeatLastCmd.html"""
        
        
        new_index = self.select_range_no + self.direction
        if new_index >= 0 and new_index < len(self.possible_ranges):
            self.select_range_no  = new_index
        if self.mark_selection:
            app.set_selection(range=self.possible_ranges[self.select_range_no], 
                              cursor_at=self.cursor_at, buff_name=self.buff_name)
        else:
            app.goto_range(self.possible_ranges[self.select_range_no], self.cursor_at, 
                           buff_name=self.buff_name)

        
class ActionInsert(Action):
    """Action that inserts and indents code at cursor position
        
    **INSTANCE ATTRIBUTES**
        
    *STR code_bef=''* -- code to be inserted before the cursor
    *STR code_after=''* -- code to be inserted after the cursor
    *INT spacing=0* -- spacing flags governing spacing before and after
    code_bef, from SpacingState (CURRENTLY IGNORED)
    *STR prefer* -- name of SymBuilder preferred for a symbol following code_bef
    *STR expect* -- identifier type expected for a symbol following code_bef
        (ignored if prefer is specified)  

    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
        
    def __init__(self, code_bef='', code_after='', spacing = 0,
                 prefer = None, expect = None, **args_super):
        self.deep_construct(ActionInsert, \
                            {'code_bef': code_bef, \
                             'code_after': code_after,
                             'spacing': spacing,
                             'prefer': prefer,
                             'expect': expect}, \
                            args_super, \
                            {})
        
        
    def execute(self, app, cont, state = None):
        """See [Action.execute].
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute"""

        debug.trace('ActionInsert.execute' ,'self=%s, self.code_bef=%s, self.code_after=%s' % (self, self.code_bef, self.code_after))

        inserted = app.insert_indent(self.code_bef, self.code_after)
        if state:
            styling_state = state.styling_state()
            if self.prefer:
                styling_state.prefer(self.prefer)
            elif self.expect:
                styling_state.expect(self.expect)
        return inserted

    def doc(self):
        """
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

        if getattr(self, 'docstring', None) != None:
            the_doc = self.docstring
        else:
            the_doc = 'Inserts \'%s\' in current buffer' % (self.code_bef + '^' + self.code_after)
        return the_doc


class ActionIncrIndentation(ActionRepeatable):
    """Action that increases the indent level of selected code by a certain
    number of level.
        
    **INSTANCE ATTRIBUTES**
        
    *INT* levels=1 -- number of levels to increase indentation by.
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
        
    def __init__(self, levels=1, **args_super):
        self.deep_construct(ActionIncrIndentation,
                            {'levels': levels},
                            args_super,
                            {})
        
        
    def execute(self, app, cont, state = None):
        """See [Action.execute].
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute"""
        
        app.incr_indent_level(levels=self.levels)


    def doc(self):
        """
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

        if getattr(self, 'docstring', None) != None:
            the_doc = self.docstring
        else:
            the_doc = 'Increase indentation of selected code by %s levels' % self.levels
        return the_doc


class ActionDecrIndentation(ActionRepeatable):
    """Action that decreases the indent level of selected code by a certain
    number of level.
        
    **INSTANCE ATTRIBUTES**
        
    *INT* levels=1 -- number of levels to decrease indentation by.
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
        
    def __init__(self, levels=1, **args_super):
        self.deep_construct(ActionDecrIndentation,
                            {'levels': levels},
                            args_super,
                            {})
        
        
    def execute(self, app, cont, state = None):
        """See [Action.execute].
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute"""
        
        app.decr_indent_level(levels=self.levels)


    def doc(self):
        """
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """
        
        if getattr(self, 'docstring', None) != None:
            the_doc = self.docstring
        else:
            the_doc = 'Decrease indentation of selected code by %s levels' % self.levels
        return the_doc    

class ActionAutoIndent(Action):
    
    """Action that automatically indents code by an appropriate amount of
    spaces.
        
    **INSTANCE ATTRIBUTES**
        
    *none* --

    CLASS ATTRIBUTES**
    
    *none* -- 
    """
        
    def __init__(self, levels=1, **args_super):
        self.deep_construct(ActionAutoIndent,
                            {},
                            args_super,
                            {})
        
        
    def execute(self, app, cont, state = None):
        """See [Action.execute].
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute"""
        
        app.indent()


    def doc(self):
        """
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

        if getattr(self, 'docstring', None) != None:
            the_doc = self.docstring
        else:
            the_doc = 'Indent selected code by appropriate amount'
        return the_doc    


class ActionDeleteCurrentLine(ActionRepeatable):
    """Delete line at cursor.

    **INSTANCE ATTRIBUTES**
        
    *none* --

    CLASS ATTRIBUTES**
        
    *none* -- 
    """
        
    def __init__(self, **args_super):
        self.deep_construct(ActionDeleteCurrentLine, 
                            {}, 
                            args_super, 
                            {})

    def doc(self):
        """See [Action.doc].

        .. [Action.doc] file:///./actions_gen.Action.html#doc
        """        
        return """Delete line at cursor."""

    def execute(self, app, cont, state = None):
        """See [Action.execute].

        .. [Action.execute] file:///./actions_gen.Action.html#execute"""
        debug.trace('ActionDeleteCurrentLine.execute', "** invoked")
        return app.delete_line()

class ActionPaging(ActionBidirectionalRepeat):
    """Move up or down a page.

    **INSTANCE ATTRIBUTES**
        
    *none* --

    CLASS ATTRIBUTES**
        
    *none* -- 
    """
        
    def __init__(self, direction, n_times, **args_super):
        self.deep_construct(ActionPaging, 
                            {"direction": direction, "n_times": n_times}, 
                            args_super, 
                            {})
        self.direction = direction
        debug.trace('ActionPaging.__init__', 'direction=%s, self.direction=%s' % (direction, self.direction))

    def doc(self):
        """See [Action.doc].

        .. [Action.doc] file:///./actions_gen.Action.html#doc
        """        
        return """move up or down the page"""

    def execute(self, app, cont, state = None):
        """See [Action.execute].

        .. [Action.execute] file:///./actions_gen.Action.html#execute"""
        debug.trace('ActionPaging.execute', "** paging in direction %s, %s times" % (self.direction, self.n_times))
        success = app.move_relative_page(self.direction, self.n_times)
        return success


class ActionGotoBeginningOfLine(Action):
    """Moves cursor to beginning of current line.
        
    **INSTANCE ATTRIBUTES**

    *none* -- 

    **CLASS ATTRIBUTES**
        
    *none* -- 
    """
    def __init__(self, **args_super):
        self.deep_construct(ActionGotoBeginningOfLine, 
                            {}, 
                            args_super, 
                            {})


    def execute(self, app, cont, state = None):
        """See [Action.execute].

        .. [Action.execute] file:///./actions_gen.Action.html#execute"""

        return app.goto(app.beginning_of_line())

    def doc(self):
        """See [Action.doc].

        .. [Action.doc] file:///./actions_gen.Action.html#doc
        """
        return "Move cursor to beginning of current line."


class ActionGotoEndOfLine(Action):
    """Moves cursor to end of current line.
        
    **INSTANCE ATTRIBUTES**

    *none* -- 

    **CLASS ATTRIBUTES**
        
    *none* -- 
    """
    def __init__(self, **args_super):
        self.deep_construct(ActionGotoEndOfLine, 
                            {}, 
                            args_super, 
                            {})


    def execute(self, app, cont, state = None):
        """See [Action.execute].

        .. [Action.execute] file:///./actions_gen.Action.html#execute"""

        return app.goto(app.end_of_line())

    def doc(self):
        """See [Action.doc].

        .. [Action.doc] file:///./actions_gen.Action.html#doc
        """
        return "Move cursor to end of current line."

class ActionGotoTopOfBuffer(Action):
    """Moves cursor to beginning of current file.
        
    **INSTANCE ATTRIBUTES**

    *none* -- 

    **CLASS ATTRIBUTES**
        
    *none* -- 
    """
    def __init__(self, **args_super):
        self.deep_construct(ActionGotoTopOfBuffer, 
                            {}, 
                            args_super, 
                            {})


    def execute(self, app, cont, state = None):
        """See [Action.execute].

        .. [Action.execute] file:///./actions_gen.Action.html#execute"""

        return app.goto(0)

    def doc(self):
        """See [Action.doc].

        .. [Action.doc] file:///./actions_gen.Action.html#doc
        """
        return "Move cursor to beginning of current file."


class ActionGotoBottomOfBuffer(Action):
    """Moves cursor to end of current buffer.
        
    **INSTANCE ATTRIBUTES**

    *none* -- 

    **CLASS ATTRIBUTES**
        
    *none* -- 
    """
    def __init__(self, **args_super):
        self.deep_construct(ActionGotoBottomOfBuffer, 
                            {}, 
                            args_super, 
                            {})


    def execute(self, app, cont, state = None):
        """See [Action.execute].

        .. [Action.execute] file:///./actions_gen.Action.html#execute"""

        return app.goto(app.len() - 1)

    def doc(self):
        """See [Action.doc].

        .. [Action.doc] file:///./actions_gen.Action.html#doc
        """
        return "Move cursor to end of current file."
    

class ActionSearch(ActionBidirectional):
    """Moves cursor to occurence of a regular expression.
        
    **INSTANCE ATTRIBUTES**
        
    *STR* regexp=None -- Regular expression to search for.
    
    *ANY* where=1 -- Place where to put cursor w.r.t. to
    *occurence. *Positive* means after occurence, *negative* means
    *before.

    *INT* num=1 -- Search for *num*th occurence.
    
    *BOOL* include_current_line -- if true, include the entire
    current line in the search (start at end of line if going
    backwards, start at beginning of line if going forwards)

    CLASS ATTRIBUTES**
        
    *none* -- 
    """
        
    def __init__(self, regexp=None, num=1, where=1, 
                 include_current_line=0, unlogged = 0,
                 **args_super):
        self.deep_construct(ActionSearch, 
                            {'regexp': regexp, 
                             'num': num, 
                             'where': where,
                             'include_current_line': include_current_line,
                             'unlogged': 0}, 
                            args_super, 
                            {})


    def execute(self, app, cont, state = None):
        """See [Action.execute].

        .. [Action.execute] file:///./actions_gen.Action.html#execute"""

        debug.trace('ActionSearch.execute', 'called, app=%s' % app)
        return app.search_for(regexp=self.regexp, direction=self.direction,
                              num=self.num, where=self.where,
                              include_current_line = self.include_current_line,
                              unlogged  = self.unlogged)

    def doc(self):
        """See [Action.doc].

        .. [Action.doc] file:///./actions_gen.Action.html#doc
        """
        
        if getattr(self, 'docstring', None) != None:
            the_doc = self.docstring
        else:
            if self.num == 1:
                if self.direction > 0:
                    str_occurence = 'to next occurence'
                else:
                    str_occurence = 'to previous occurence'
            else:
                if self.direction > 0:
                    str_dir = 'forward'
                else:
                    str_dir = 'backward'                
                str_occurence = '%s to occurence #%s' % (str_dir, self.num)
            if self.where > 0:
                str_where = ''
            else:
                str_where = ' Puts cursor after occurence.'
                
            the_doc = 'Moves %s of %s.%s' % (str_occurence, self.regexp, str_where)
        return the_doc


class ActionSearchBidirectionalRepeat(ActionSearch, ActionBidirectionalRepeat):
    """Moves cursor to occurence of a regular expression.

    Contrarily to [ActionSearch], this class of search can be repeated
    or qualified by subsequent commands (e.g. 'again 3 times',
    'previous one').
        
    **INSTANCE ATTRIBUTES**
        
    *none* --

    CLASS ATTRIBUTES**
        
    *none* -- 

    ..[ActionSearch] file:///./actions_gen.ActionSearch.html#ActionSearch"""
        
    def __init__(self, **args_super):
        self.deep_construct(ActionSearchBidirectionalRepeat, 
                            {}, 
                            args_super, 
                            {})

    def doc(self):
        """See [Action.doc].

        .. [Action.doc] file:///./actions_gen.Action.html#doc
        """        
        return ActionSearch.doc(self) + """ (can be repeated or qualified by subsequent utterance like: 'do that again' and 'previous one')."""

class FakeActionSearchOrLookback(ActionSearch, ActionBidirectionalRepeat):
    """Moves cursor to occurence of a regular expression, using
    lookback_search for backwards searches (only valid for simple
    regular expressions) but search_for for forward searches.

    Contrarily to [ActionSearch], this class of search can be repeated
    or qualified by subsequent commands (e.g. 'again 3 times',
    'previous one').
        
    **INSTANCE ATTRIBUTES**
        
    *none* --

    CLASS ATTRIBUTES**
        
    *none* -- 

    ..[ActionSearch] file:///./actions_gen.ActionSearch.html#ActionSearch"""
        
    def __init__(self, extra_space = 0, **args_super):
        self.deep_construct(FakeActionSearchOrLookback, 
                            {'extra_space': extra_space}, 
                            args_super, 
                            {})
        if extra_space:
            space = '[ \t]{0,1}'
            if self.where > 0:
                self.regexp = self.regexp + space
            else:
                self.regexp = space + self.regexp 

class ActionSearchOrLookback(ActionSearch, ActionBidirectionalRepeat):
    """Moves cursor to occurence of a regular expression, using
    lookback_search for backwards searches (only valid for simple
    regular expressions) but search_for for forward searches.

    Contrarily to [ActionSearch], this class of search can be repeated
    or qualified by subsequent commands (e.g. 'again 3 times',
    'previous one').
        
    **INSTANCE ATTRIBUTES**
        
    *none* --

    CLASS ATTRIBUTES**
        
    *none* -- 

    ..[ActionSearch] file:///./actions_gen.ActionSearch.html#ActionSearch"""
        
    def __init__(self, extra_space = 0, **args_super):
        self.deep_construct(ActionSearchOrLookback, 
                            {'extra_space': extra_space}, 
                            args_super, 
                            {})
        
    def doc(self):
        """See [Action.doc].

        .. [Action.doc] file:///./actions_gen.Action.html#doc
        """        
        return ActionSearch.doc(self) + """ (can be repeated or qualified by subsequent utterance like: 'do that again' and 'previous one')."""

    def skip_space(self, buff):
        if self.where == 1:
            if buff.looking_at('[ \t]'):
                buff.move_relative(1)
                return 1
        else:
            pos = buff.cur_pos()
            if pos and buff.looking_at('[ \t]', pos - 1):
                buff.move_relative(-1)
                return 1
        return 0

    def execute(self, app, cont, state = None):
        """See [Action.execute].

        .. [Action.execute] file:///./actions_gen.Action.html#execute"""

        debug.trace('ActionSearchOrLookback.execute', '** invoked, self.direction=%s' % self.direction)
        buff = app.find_buff()
        if debug.tracing('ActionSearchOrLookback.execute'):
            debug.trace('ActionSearchOrLookback.execute',
                            'pos = %d' % buff.cur_pos())
        if self.direction > 0:
            best_match = buff.search_for_match(regexp = self.regexp, 
                direction = self.direction, num = self.num, 
                where = self.where)
            if debug.tracing('ActionSearchOrLookback.execute'):
                if best_match:
                    debug.trace('ActionSearchOrLookback.execute',
                        'best_match = %d, %d' % best_match)
                else:
                    debug.trace('ActionSearchOrLookback.execute',
                        'no match found')
        else:
            best_match = buff.lookback_search(regexp = self.regexp, 
                num = self.num, where = self.where, unlogged = 1)
        alt_pos = None
        if best_match:
            new_cur_pos = buff.pos_extremity(best_match, self.where)
            buff.goto(new_cur_pos)
            if self.extra_space:
                if self.skip_space(buff):
                    alt_pos = buff.cur_pos()

        buff.log_search(self.regexp, self.direction, self.where,
            best_match, alt_pos = alt_pos)
        if best_match:
            return 1
        return 0


class ActionBackspace(ActionRepeatable):
    """Backspace a certain number of times.

        
    **INSTANCE ATTRIBUTES**
        
    INT *n_times* -- number of times to backspace.

    CLASS ATTRIBUTES**
        
    *none* -- 

    """
        
    def __init__(self, n_times=1, **args_super):
        self.deep_construct(ActionBackspace, 
                            {'n_times': n_times}, 
                            args_super, 
                            {})

    def doc(self):
        """See [Action.doc].

        .. [Action.doc] file:///./actions_gen.Action.html#doc
        """        
        return "Backspace %s times." % self.n_times

    def execute(self, app, cont, state = None):
        """See [Action.execute].

        .. [Action.execute] file:///./actions_gen.Action.html#execute"""

        app.backspace(self.n_times)


class ActionFuncCallWithParens(Action):
    """Inserts a call to a function or method. Assumes that the syntax for calls is
    funcName(args).

     **INSTANCE ATTRIBUTES**
        
     STR *args_list_type* -- indicates the type of arg list ('empty' or 'non-empty').
    
     STR *func_name* -- name of the function or method name.

     CLASS ATTRIBUTES**
        
     *none* -- 
   
    """
    def __init__(self, func_name, args_list_type, **args_super):
        debug.trace('ActionFuncCallWithParens.__init__', '** args_list_type=%s' % args_list_type)
        self.deep_construct(ActionFuncCallWithParens, 
                            {'func_name': func_name,
                             'args_list_type': args_list_type}, 
                            args_super, 
                            {})

    def doc(self):
        """See [Action.doc].

        .. [Action.doc] file:///./actions_gen.Action.html#doc
        """        
        return "Prints a call to function/method %s with %s arguments list." % (self.func_name, self.args_list_type)

    def execute(self, app, cont, state = None):
        """See [Action.execute].

        .. [Action.execute] file:///./actions_gen.Action.html#execute"""

        print "-- ActionFuncCallWithParens.execute: invoked"

        if self.args_list_type == 'non-empty':
           tmp_insert_action = ActionInsert(code_bef="%s(" % self.func_name, code_after=")")
        else:
           tmp_insert_action = ActionInsert(code_bef="%s()" % self.func_name, code_after="")
        tmp_insert_action.execute(app, cont, state)

class ActionInsertNewClause(Action):

    """Inserts a new clause in some multiple clause statement like
    "if" or "try".
        
    **INSTANCE ATTRIBUTES**
        
    STR *end_of_clause_regexp=None* -- Use this regexp to find the end
    of the current clause.

    STR *start_of_next_clause_regexp = None* -- If this regexp occurs
    closer to the cursor than end_of_clause_regexp, insert relative to
    this regexp instead of end_of_clause_regexp.  Also reverse sign of
    *where* in this case.

    INT *add_lines* -- Number of lines to insert between end of current clause
    and the new one.
        
    INT *back_indent_by* -- Number of levels to back indent the new
    clause w.r.t to the body of the current clause.
        
    STR *code_bef=''* -- Part of the new clause to be inserted before cursor.

    STR *code_after=''* -- Part of the new clause to be inserted after cursor.
    
    INT *where = -1* -- If > 0, then put cursor after the occurence of *end_of_clause_regexp*,
    otherwise put it before.
    
    INT *direction = 1* -- If > 0, then search forward for the occurence of *end_of_clause_regexp*,
    otherwise search backwards.
        
    *BOOL* include_current_line -- if true, include the entire
    current line in the search (start at end of line if going
    backwards, start at beginning of line if going forwards)
    
    CLASS ATTRIBUTES**
        
    *none* -- 
    """
        
    def __init__(self, end_of_clause_regexp, start_of_next_clause_regexp='', code_bef='',
                 code_after='', add_lines=1, back_indent_by=1, where = -1, direction = 1,
                 include_current_line = 0,
                 **args_super):
        
        self.deep_construct(ActionInsertNewClause, 
                            {'end_of_clause_regexp': end_of_clause_regexp, 
                             'start_of_next_clause_regexp': start_of_next_clause_regexp, 
                             'where': where,
                             'direction': direction,
                             'add_lines': add_lines, 
                             'code_bef': code_bef, 
                             'code_after': code_after,
                             'back_indent_by': back_indent_by,
                             'include_current_line': include_current_line}, 
                            args_super, 
                            {})


    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
                        
        
        debug.trace('ActionInsertNewClause.execute',
            'where = %d, direction = %d, expression = %s' % \
            (self.where, self.direction,
            repr(self.end_of_clause_regexp)))
        debug.trace('ActionInsertNewClause.execute',
            'before search, pos = %d' % app.cur_pos())

        orig_pos = app.cur_pos()

        app.search_for(regexp=self.end_of_clause_regexp, 
                       where = self.where, direction = self.direction,
                       include_current_line = self.include_current_line,
                       unlogged=1)
        debug.trace('ActionInsertNewClause.execute',
            'after search, pos = %d' % app.cur_pos())
        next_clause_pos = app.cur_pos()

        if self.start_of_next_clause_regexp != '':
            # see if next clause starts before we find the token ending
            # the current clause (this is required for C/C++, so that
            # saying 'new statement' between an empty brace pair does not
            # search beyond the close-brace
            debug.trace('ActionInsertNewClause.execute',
                        'where = %d, direction = %d, expression = %s' % \
                        (-1*self.where, self.direction,
                         repr(self.start_of_next_clause_regexp)))

            app.goto(orig_pos)
            app.search_for(regexp=self.start_of_next_clause_regexp, 
                           where = -1*self.where, direction = self.direction,
                           include_current_line = self.include_current_line,
                           unlogged=1)
            debug.trace('ActionInsertNewClause.execute',
                'after start-of-next-clause search, orig_pos = %d, next_clause_pos = %d, pos = %d'
                        % (orig_pos, next_clause_pos, app.cur_pos()))

            if app.cur_pos() == orig_pos or \
                   (self.where > 0 and app.cur_pos() >= next_clause_pos) or \
                   (self.where <= 0 and app.cur_pos() <= next_clause_pos):
                debug.trace('ActionInsertNewClause.execute',
                            'inserting at end_of_clause_regexp')
                # go back to position found by end_of_clause_regexp
                app.goto(next_clause_pos)
            else:
                # insert here instead
                debug.trace('ActionInsertNewClause.execute',
                            'inserting at start_of_next_clause_regexp')
            
        debug.trace('ActionInsertNewClause.execute',
            'inserting at ' + `app.cur_pos()`)

        blank_lines = ""
        for ii in range(self.add_lines):
            blank_lines = "%s%s" % (blank_lines, app.pref_newline_convention())

        if self.direction > 0:
           code_bef = blank_lines
           code_after = ""
        else:
           code_bef = ""
           code_after = blank_lines
                       
        debug.trace('ActionInsertNewClause.execute',
            'about to insert %s, %s' % (repr(code_bef),
            repr(code_after)))
        app.insert_indent(code_bef=code_bef, code_after=code_after)
            
        #
        # Client-side automatic indentation is usually smart enough to know
        # if a new clause should be backindented or not. 
        # But our language-indenpendant server-side indentation needs to be 
        # told more explicitely. 
        #
        if self.back_indent_by > 0 and app.curr_buffer().uses_server_side_indent():
            debug.trace('ActionInsertNewClause.execute',
                'about decrease indent by %d' % self.back_indent_by)
            app.decr_indent_level(levels=self.back_indent_by)
            
        debug.trace('ActionInsertNewClause.execute',
            'about to insert %s, %s' % (repr(self.code_bef),
            repr(self.code_after)))
        app.insert_indent(code_bef=self.code_bef, code_after=self.code_after)
        debug.trace('ActionInsertNewClause.execute',
            'returning')


class ActionInsertAround(Action):

    """Inserts text before and after an expression where the cursor lies.  May be used
    for example to surround an expression with parentheses.  The cursor position is
    not changed.  No server-side indentation is done (this is intended for surrounding
    small expressions, not multi-line constructs).
        
    **INSTANCE ATTRIBUTES**
        
    STR *start_regexp=None* -- Use this regexp to find the expression
    before which code_bef will be inserted.

    STR *end_regexp=None* -- Use this regexp to find the expression
    after which code_after will be inserted.

    STR *code_bef=''* -- New text to be inserted before start_regexp.

    STR *code_after=''* -- New text to be inserted after end_regexp.
    
    CLASS ATTRIBUTES**
        
    *none* -- 
    """
        
    def __init__(self, start_regexp, end_regexp,
                 code_bef='',
                 code_after='', **args_super):
        
        self.deep_construct(ActionInsertAround, 
                            {'start_regexp': start_regexp,
                             'end_regexp': end_regexp, 
                             'code_bef': code_bef, 
                             'code_after': code_after},
                             args_super, 
                             {})

    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
                        
        debug.trace('ActionInsertAround.execute',
            'start = %s, end = %s' % \
            (repr(self.start_regexp), repr(self.end_regexp)))
        debug.trace('ActionInsertAround.execute',
            'before search, pos = %d' % app.cur_pos())

        orig_pos = app.cur_pos()

        # search backward to start_regexp and insert code_bef, before cursor
        app.search_for(regexp=self.start_regexp, 
                       where = -1, direction = -1,
                       unlogged = 1)
        app.insert_indent(self.code_bef,'')
        
        # return to orig_pos before searching forward
        app.goto(orig_pos + len(self.code_bef))

        # search backward to start_regexp and insert code_after, after cursor

        app.search_for(regexp=self.start_regexp, 
                       where = 1, direction = 1,
                       unlogged = 1)
        app.insert_indent('',self.code_after)

        # return to original cursor position - how do we know how much was inserted, including spaces?
        app.goto(orig_pos + len(self.code_bef))

        debug.trace('ActionInsertAround.execute',
                    'after insertion, pos = %d' % app.cur_pos())



class ActionCompileSymbols(Action):
    """Compiles symbols from a buffer.
    
    **INSTANCE ATTRIBUTES**
        
    STR *buff_name* -- name of the source buffer from which to compile symbols. If *None*, then
    use the active buffer.
        
    CLASS ATTRIBUTES**
        
    *none* -- 
    """

    def __init__(self, buff_name=None, alt_contents = None, language = None, 
            **args_super):
        """
        **INPUTS**

        *STR* buff_name -- name of the buffer

        *STR* alt_contents -- alternative string to use instead of the
        contents of the buffer

        *STR* language -- if alt_contents is specified, then so must the
        language.  Otherwise, this parameter is ignored.
        """
        self.deep_construct(ActionCompileSymbols, 
                            {'buff_name': buff_name,
                             'language': language, 
                             'alt_contents': alt_contents}, 
                                args_super, 
                            {})
                            
    def doc(self):
        return 'Compiles symbols from a buffer';
    
                            

    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
        
        manager = app.current_manager()
        if not manager:
            return
        interpreter = manager.interpreter()
        if self.alt_contents and self.language:
            contents = self.alt_contents
            language = self.language
        else:
            buffer = app.find_buff(self.buff_name)
            contents = buffer.contents()
            language = buffer.language_name()
        interpreter.user_message("Compiling symbols for %s" % self.buff_name, 
            instance = app.name())
        interpreter.parse_symbols(contents, language)
        interpreter.user_message("Done compiling symbols", 
                instance = app.name())


class ActionPrintSymbols(Action):
    """Print the list of all known symbols.
    
    **INSTANCE ATTRIBUTES**
        
    *none*
        
    CLASS ATTRIBUTES**
        
    *none* -- 
    """

    def __init__(self, buff_name=None, **args_super):
        self.deep_construct(ActionPrintSymbols, \
                            {}, \
                                args_super, \
                            {})
                            
    def doc(self):
        return 'Print list of all known symbols.';
    
                            

    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
        
        manager = app.current_manager()
        if manager:
           try:
             manager.interpreter().print_symbols()
           except AttributeError:
             print 'print symbols command not supported with old Mediator'

class ActionPrintAbbrevs(Action):
    """Print the list of all abbreviations (resolved and unresolved).
    
    **INSTANCE ATTRIBUTES**
        
    *none*
        
    CLASS ATTRIBUTES**
        
    *none* -- 
    """

    def __init__(self, buff_name=None, **args_super):
        self.deep_construct(ActionPrintAbbrevs, \
                            {}, \
                                args_super, \
                            {})
                            
    def doc(self):
        return 'Print list of abbreviations.';
    
                            

    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
        
        manager = app.current_manager()
        if manager:
           try:
             manager.interpreter().print_abbreviations(1)
           except AttributeError:
             print 'print abbreviations command not supported with old Mediator'    
    

class ActionTypeText(Action):
    """Types text to the active window.
    
    Don't use this action unless you absolutely have to! Instead, use ActionInsert.
    
    **INSTANCE ATTRIBUTES**
                
    CLASS ATTRIBUTES**
        
    *none* -- 
    """

    def __init__(self, key_strokes, **args_super):
        self.deep_construct(ActionTypeText, \
                            {'key_strokes': key_strokes}, \
                                args_super, \
                            {})
                            
    def doc(self):
        return 'Sends keystrokes: "%s" to the active window';
                               
    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
        
        sr_interface.send_keys(self.key_strokes)

class ActionDeleteSelectedText(Action):
    """Delete selected text.
        
    **INSTANCE ATTRIBUTES**
                
    CLASS ATTRIBUTES**
        
    *none* -- 
    """

    def __init__(self, **args_super):
        self.deep_construct(ActionDeleteSelectedText, \
                            {}, \
                                args_super, \
                            {})
                            
    def doc(self):
        return 'Delete selected text';
                               
    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
        
        app.delete(app.get_selection())

class ActionCopySelectedText(Action):
    """Copies selected text.
        
    **INSTANCE ATTRIBUTES**
                
    CLASS ATTRIBUTES**
        
    *none* -- 
    """

    def __init__(self, **args_super):
        self.deep_construct(ActionCopySelectedText, \
                            {}, \
                                args_super, \
                            {})
                            
    def doc(self):
        return 'Copy selected text';
                               
    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
        debug.trace('ActionCopySelectedText.execute', '** INVOKED')
        app.copy_selection()

class ActionCutSelectedText(Action):
    """Cuts selected text.
        
    **INSTANCE ATTRIBUTES**
                
    CLASS ATTRIBUTES**
        
    *none* -- 
    """

    def __init__(self, **args_super):
        self.deep_construct(ActionCutSelectedText, \
                            {}, \
                                args_super, \
                            {})
                            
    def doc(self):
        return 'Cuts selected text';
                               
    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
        app.cut_selection()


class ActionCopyLine(Action):
    """Copies line at cursor.
        
    **INSTANCE ATTRIBUTES**
                
    CLASS ATTRIBUTES**
        
    *none* -- 
    """

    def __init__(self, **args_super):
        self.deep_construct(ActionCopyLine, \
                            {}, \
                                args_super, \
                            {})
                            
    def doc(self):
        return 'Copy line that the cursor is currently on.';
                               
    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
        app.select_line()
        app.copy_selection()

class ActionCutLine(Action):
    """Cuts current line.
        
    **INSTANCE ATTRIBUTES**
                
    CLASS ATTRIBUTES**
        
    *none* -- 
    """

    def __init__(self, **args_super):
        self.deep_construct(ActionCutLine, \
                            {}, \
                                args_super, \
                            {})
                            
    def doc(self):
        return 'Cuts current line.';
                               
    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
        app.select_line()
        app.cut_selection()


class ActionPasteClipboard(Action):
    """Paste content of clipboard into current buffer.
        
    **INSTANCE ATTRIBUTES**
                
    CLASS ATTRIBUTES**
        
    *none* -- 
    """

    def __init__(self, **args_super):
        self.deep_construct(ActionPasteClipboard, \
                            {}, \
                                args_super, \
                            {})
                            
    def doc(self):
        return 'Paste content of clipboard into current buffer';
                               
    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
        debug.trace('ActionPasteClipboard.execute', '** INVOKED')
        app.paste()

class ActionSwitchTranslation(Action):
    """Turns translation on/off
        
    **INSTANCE ATTRIBUTES**
        
    *BOOL* on -- If true, set translation to 'on'
        
    CLASS ATTRIBUTES**
        
    *none* -- 
    """
        
    def __init__(self, on=None, **args_super):
        self.deep_construct(ActionSwitchTranslation, \
                            {'on': on}, \
                                args_super, \
                            {})

    def execute(self, app, cont, state = None):
        """See [Action.execute] for details.
        
        .. [Action.execute] file:///./Action.Action.html#execute"""
        
        app.translation_is_off = not on


    def doc(self):
        if self.on:
            state = 'on'
        else:
            state = 'off'
        return 'turns translation \'%s\'' % state


#
# Index of the current anonymous action function.
#
anonymous_action_curr_num = 0

class AnonymousAction(exceptions.Exception):
    """Raised when the code for an anonymous action function has syntax errors"""

def indent_code(num, code):
    padding = ''
    for ii in range(num): padding = padding + ' '
    code = re.sub('(^|\n)', '\\1' + padding, code)
    return code

def anonymous_action(code, description):
    """This function creates an anonymous action function.

    The anonymous function gets an internal name of the form
    *anonymous_action_N* where *N* is some number.

    *STR code* -- Code for the anoymous action function. This code
     should assume that the application object is called *app*, and
     the context object is called *cont*.

    *STR description* -- Description of the action function
    """

    global anonymous_action_curr_num

    #
    # Create code for a new anymous action function *anonymous_action_N*
    # in a temporary file (for some reason, *exec* can't compile code
    # that declares a new function, whereas *execfile* can)
    #
    tmp_file_name = vc_globals.tmp + os.sep + 'tmp_anonymous_action.py'
    tmp_file = open(tmp_file_name, 'w')
    tmp_file.write('def anonymous_action_%s(app, cont):\n%s\n%s' % (anonymous_action_curr_num, indent_code(4, '"""' + description + '"""'), indent_code(4, code)))
    tmp_file.close()

    try:
        execfile(tmp_file_name)
        fct = eval('anonymous_action_%s' % anonymous_action_curr_num)
        anonymous_action_curr_num = anonymous_action_curr_num + 1
        return fct        
    except:
        raise AnonymousAction, 'Syntax error in code of anonymous action function.\Code was:\n%s\n' % code


class ActionInsertTemplate(ActionInsert):
    """Action that inserts a code template at the cursor location
        
    **INSTANCE ATTRIBUTES**
        
    *STR template_name -- name of the template to be inserted
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
        
    def __init__(self, language_name, template_name, **args_super):
        self.deep_construct(ActionInsertTemplate, \
                            {'language_name': language_name, \
                             'template_name': template_name}, \
                            args_super, \
                            {})
        
        
    def execute(self, app, cont, state = None):
        """See [Action.execute].
        .. [Action.execute] file:///./actions_gen.Action.html#execute"""

        print "-- ActionInsertTemplate.execute: self.language_name=%s - self.template_name=%s" % (cont.language, self.template_name)

        # TODO: Emacs specific template management, make it generic
        # TODO: have languages share templates

        # Template Lisp functions should be on the format
        # 'snippet-abbrev-<language_name>-mode-<template_name>' 
        # e.g. f.e. 'snippet-abbrev-scala-mode-for'
        #key_strokes='{Esc}xsmart-snippet-abbrev-' + self.language_name + '-mode-' + self.template_name + '{Enter}'

        # Using YASnippets
        key_strokes = self.template_name + '{Esc}xyas/expand{Enter}'
        ActionTypeText(key_strokes = key_strokes).execute(app, cont, state)

        return self.template_name

    def doc(self):
        """
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

        if getattr(self, 'docstring', None) != None:
            the_doc = self.docstring
        else:
            the_doc = 'Inserts template code named \'%s\' at the cursor.' % self.template_name
        return the_doc

class ActionTemplateManagement(ActionInsert):
    """Action that manages the YASnippet templates
        
    **INSTANCE ATTRIBUTES**
        
    *STR command -- name of the command
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
        
    def __init__(self, command, **args_super):
        self.deep_construct(ActionTemplateManagement, \
                            {'command': command}, \
                            args_super, \
                            {})
        
        
    def execute(self, app, cont, state = None):
        #key_strokes='{Esc}xsnippet-' + self.command + '{Enter}'
        
        # using YASnippets
        key_strokes='{Esc}xyas/' + self.command + '{Enter}'
        ActionTypeText(key_strokes = key_strokes).execute(app, cont, state)

        # send the buffer change to the update queue
        key_strokes='{Esc}xvcode-queue-current-buffer-change{Enter}'
        ActionTypeText(key_strokes = key_strokes).execute(app, cont, state)

    def doc(self):
        """
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

        if getattr(self, 'docstring', None) != None:
            the_doc = self.docstring
        else:
            the_doc = 'manages the snippet templates'
        return the_doc


###############################################################################
# Specific instances of generic actions
###############################################################################

gen_parens_pair = \
    ActionInsert(code_bef='(', code_after=')',
                 docstring="""Insert parens and puts cursor in between""")

gen_empty_parens_pair = \
    ActionInsert(code_bef='()', code_after='',
                 docstring="""Insert parens and puts cursor after them.""")
                 
gen_brackets_pair = \
    ActionInsert(code_bef='[', code_after=']',
                 docstring="""Insert brackets""")

gen_braces_pair = \
    ActionInsert(code_bef='{', code_after='}',
                 docstring="""Insert braces""")

gen_quotes_pair = \
    ActionInsert(code_bef='"', code_after='"',
                 docstring="""Insert quotes and moves cursor in between""")

gen_single_quotes_pair_after = \
    ActionInsert(code_bef='""', code_after='',
                 docstring="""Insert single quotes and moves cursor after""")

gen_single_quotes_pair = \
    ActionInsert(code_bef='\'', code_after='\'',
                 docstring="""Insert single quotes and moves cursor in between""")

gen_single_quotes_pair_after = \
    ActionInsert(code_bef='\'\'', code_after='',
                 docstring="""Insert single quotes and moves cursor after""")

gen_translation_off = \
    ActionSwitchTranslation(0)

gen_translation_on = \
    ActionSwitchTranslation(1)

    
