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

"""Action functions for Python language"""

import re, sys

import SymDict
from actions_gen import Action, ActionInsert, ActionSearch, ActionInsertNewClause


py_empty_dictionary = ActionInsert(code_bef='{}', code_after='',
                                   docstring="""Types code for an empty Python dictionary (e.g. {}^)""")

py_simple_for = ActionInsert(code_bef='for ', code_after=':\n\t',
                             docstring="""Insert template code for a simple Python for loop""")

py_goto_body = \
    ActionSearch(regexp=':[ \t]*(\n|$)[ \t]*',  
                 docstring="""Move cursor to the body of a Python compound statement""")

py_new_statement = \
    ActionInsertNewClause(end_of_clause_regexp='(\n|$)', 
                          where = -1, direction = 1,
                          add_lines = 1,
                          code_bef='', code_after='',
                          back_indent_by=0,
                          docstring = """Inserts a new line below current one""")
                           
py_new_statement_above = \
    ActionInsertNewClause(end_of_clause_regexp='(^|\n)', 
                          where = 1, direction=-1,
                          add_lines = 1, 
                          code_bef='', code_after='', back_indent_by=0,
                          docstring = """Inserts a new line below current one""")
                           

py_class_definition = \
    ActionInsert(code_bef='class ', code_after=':\n\t',
                 docstring="""Inserts template code for a Python class""",
                 expect = "class")

py_subclass = \
    ActionInsert(code_bef='(', code_after=')',
                 docstring="""Inserts empty parens where to type name of superclass.""",
                 expect = "class")

class ActionPyInsertInBody(Action):
    """Inserts a new line at the start of the block of the previous or 
    following compound statement"""
    def __init__(self, direction = 1, **args_super):
        self.deep_construct(ActionPyInsertInBody,
                            {'direction': direction},
                            args_super)
    def execute(self, app, cont, state = None):
        """See [Action.execute].
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute"""
        match = app.search_for(regexp=':.*', direction = self.direction,
            unlogged = 1)
        if not match:
            return
# check if the next line is blank.  If the previous search succeeded, then 
# we should be at the end of the line containing the ":" and the next
# character should be a newline (unless we are at the end of the buffer).  
# In either case, this next search, with where = -1, should not move the
# cursor.
        match = app.search_for('\n[ \t]*(\n|$)', where = -1, unlogged = 1)
        if not match:
# if no blank line, insert a new one and indent it appropriately
            insert_indented_line = ActionInsert(code_bef = '\n\t', 
                code_after = '')
            insert_indented_line.execute(app, cont, state = state)
        else:
# if there is a blank line, just go to the end of it 
# (and assume that it has the correct indentation)
            match = app.search_for('\n[ \t]*', unlogged = 1)

py_class_body = \
    ActionPyInsertInBody()

py_method_declaration = \
    ActionInsert(code_bef='def ', code_after='(self):\n\t',
                 docstring="""Types template code for a method""",
                 expect = "method")

py_constructor_definition = \
    ActionInsert(code_bef='def __init__(self', code_after='):\n\t',
                 docstring="""Types template code for a constructor""")


py_function_declaration = \
    ActionInsert(code_bef='def ', code_after='():\n\t',
                 docstring="""Types template code for a function""",
                 expect = 'function')

class ActionPyAddArgument(Action):
    """Positions the cursor to add arguments to a Python function call or
    definition"""
    
    def __init__(self, **args_super):
        self.deep_construct(ActionPyAddArgument, \
                            {}, \
                            args_super, \
                            {})

    def execute(self, app, cont, state = None):
        """See [Action.execute].
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute"""
        

        found = app.search_for('\\)\s*:{0,1}', where=-1, unlogged = 1)
        if found:
            #
            # See if argument list was empty
            #        
            arg_list_empty = 1
            pos = app.curr_buffer().cur_pos() - 1
            
            #
            # Find first preceding non-space character
            #
            while pos >= 0:
                if not re.match('\s', app.curr_buffer().contents()[pos]):
                    #
                    # The first preceding non-space character is (
                    #    => argument list is empty
                    #
                    arg_list_empty = app.curr_buffer().contents()[pos] == '('
                    break
                pos = pos - 1

            #
            # Insert comma if this is not the first argument in the list
            #
            if not arg_list_empty:
                app.insert_indent(', ', '')

py_function_add_argument = \
    ActionPyAddArgument()

py_function_body = \
    ActionPyInsertInBody()

    
class ActionPyCommentAbove(Action):
    """Creates a new comment line above the current one."""
    
    def __init__(self, **args_super):
        self.deep_construct(ActionPyCommentAbove, \
                            {}, \
                            args_super, \
                            {})

    def execute(self, app, cont, state = None):
        """See [Action.execute].
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute"""
        

        was_on_line = app.line_num_of()
        found = app.search_for('(^|\n)', direction=-1, where=-1, unlogged = 1)
        app.insert('\n')
        if was_on_line == 1:
           app.goto(0)
        app.insert('# ')
    
