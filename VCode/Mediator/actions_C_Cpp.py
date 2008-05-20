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


"""Action functions for C language """

from actions_gen import Action, ActionInsert, ActionSearch, ActionInsertNewClause, ActionTypeText, ActionCompound

import whrandom
import string
import debug, sys

class ActionHeaderWrapper(Action):
    """Action that inserts a code template for one time #include
        
    **INSTANCE ATTRIBUTES**
        
    *NONE*
    
    CLASS ATTRIBUTES**
    
    *none* -- 
    """
        
    def __init__(self, **args_super):
        self.deep_construct(ActionHeaderWrapper, \
                            {}, \
                            args_super, \
                            {})

        
    def execute(self, app, cont, state = None):
        """See [Action.execute].
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute"""
        unique_str = str(string.upper(string.replace(app.curr_buffer_name(),'.','_')))
        # SN removing the random part for now, unless someone really likes it...
        # + \
        # str(whrandom.randint(10**6,(10**7)-1))
        before_string  = '#ifndef ' + unique_str + '\n' + '#define ' + unique_str + '\n\n'
        after_string = '\n\n\n#endif\n'
        app.insert_indent(before_string, after_string)


    def doc(self):
        """
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

        if self.docstring != None:
            the_doc = self.docstring
        else:
            the_doc = 'inserts a code template for one time #include'
        return the_doc


class ActionCAddArgument(Action):
    """Positions the cursor to add arguments to a C function call or
    definition"""
    
    def __init__(self, **args_super):
        self.deep_construct(ActionCAddArgument, \
                            {}, \
                            args_super, \
                            {})

    def execute(self, app, cont, state = None):
        """See [Action.execute].
        
        .. [Action.execute] file:///./actions_gen.Action.html#execute"""
        

        found = app.search_for(r'\)', where=-1, unlogged = 1)
        if found:
            #
            # See if argument list was empty
            #        
            arg_list_empty = 0
            
            #
            # Find first preceding non-space character
            #
            buff = app.curr_buffer()
            pos = buff.char_search(r'\S', direction = -1)
            if buff.contents()[pos] == '(':
                arg_list_empty = 1
            buff.goto(pos+1)

            #
            # Insert comma if this is not the first argument in the list
            #
            if not arg_list_empty:
                app.insert_indent(', ', '')


c_simple_for = \
    ActionInsert(code_bef='for (',
#                code_after=';  <= ; ++)\n\t{\n\t\n\t}\n',
# I'm switching this to use the more common indentation, which matches
# what Emacs does anyway
# ugh - that's not true -- Emacs does it this way for for-loops which
# start at indentation = 0, but the way above for for-loops which are
# inside another block.  That's idiotic!

# because it's easy to say e.g. 'equals zero' and harder to correct it
# if we want to handle the loop another way, don't insert the '=0'
# automatically (SN)
#                code_after='=0;  <= ; ++)\n{\n\t\n}\n',
                 code_after='; ; )\n{\n\t\n}\n',
                 docstring = """Insert template code for a simple C for loop""")

c_simple_while = \
    ActionInsert(code_bef='while (', code_after=')\n{\n\t\n}',
                 docstring = """Insert template code for a simple C while loop""")

c_do_while = \
    ActionInsert(code_bef='do\n{\n\t', code_after='\n} while ();\n',
                 docstring = """Insert template code for a C/C++ do-while loop""")

c_goto_body = \
    ActionSearch(regexp=r'\{[ \t]*\n',
                 docstring="""Move cursor to the body of a C compound statement""")

cpp_class_definition = \
     ActionInsert(code_bef='class ',
                  code_after='\r\t{\n\n\tpublic:\n\t\nprivate:\n\t\n}',
                  docstring = """Insert template code for a C++ class""",
                  expect="class")

cpp_template_definition = \
     ActionInsert(code_bef='template <',
#                  code_after='>\nclass \r\t{\n\n\tpublic:\n\t\nprivate:\n\t\n}',
                  # implementing it without the class stuff lets us use 'define class' to insert the class stuff...
                  code_after='>\n',
                  docstring = """Insert template code for a C++ class template""",
                  expect="template")

cpp_subclass = \
    ActionInsert(code_bef=': ', code_after='',
                 docstring = """Inserts ': ' for subclass definition""", expect='class')

cpp_class_body = \
    ActionSearch(regexp=r'\{\s*',
                 docstring="""Moves cursor to the body of a class""")

c_function_declaration = \
    ActionInsert(code_bef='',
                 code_after='();',
                 docstring = """Types template code for a C function or C++ method declaration (no function body)""")

c_function_definition = \
    ActionInsert(code_bef='',
                 code_after='()\n{\n\t\n}',
                 docstring = """Types template code for a C function or C++ method (including body)""")

c_function_add_argument = \
    ActionCAddArgument(
              docstring="""Positions cursor for adding an argument to a C/C++ function declaration or call""")

c_function_body = \
    ActionSearch(regexp=r'\{\s*',
                 docstring = """Moves cursor to the body of a C/C++ method or function""")

#### NOTE: this doesn't work well if you're in a {} pair (e.g. 'if ... then new statement')
# we need different behavior when there's a } before a ; -- insert BEFORE the }
c_new_statement = \
                ActionCompound(
    (ActionInsertNewClause(
    #    end_of_clause_regexp='([;{]|(#.*$)|(#.*\\n))', 
                                      end_of_clause_regexp = "([;{]\\s*($|\\n)|#.*($|\\n))",
                                      start_of_next_clause_regexp = "\\}",
                                      where = 100, direction = 1,
                                      add_lines = 0,
                                      code_bef='', code_after='\n',
                                      back_indent_by=0,
                                      include_current_line = 1,
                                      docstring = """Start a new C/C++ statement on next line"""),
     ActionInsert(code_after=";"),
     ActionSearch(";", direction=1, where=-1)))

c_new_statement_above = \
                      ActionInsertNewClause(
    end_of_clause_regexp = r"(;.*\n)|(\{.*\n)|(\}.*\n)|(#.*($|\n))",
    # since we always insert after the found symbol (e.g. }) we don't
    # need to worry about start_of_next_clause_regexp the way we do
    # for c_new_statement
    # start_of_next_clause_regexp = "[\\}\\{]",
    where = 1, direction = -1,
    add_lines = 1,
    code_bef='\t', code_after=';',
    back_indent_by=0,
    include_current_line = 0,
    docstring = """Start a new C/C++ statement before this one""")



# this assumes the if ends with a '}' (not a one-liner)
c_else_if = \
                ActionInsertNewClause(end_of_clause_regexp='\}', 
                                      where = 1, direction = 1,
                                      add_lines = 1,
                                      code_bef='\nelse if (', code_after=')\n{\n\t\n}',
                                      back_indent_by=0,
                                      docstring='else-if clause of a C conditional')

# this assumes the if ends with a '}' (not a one-liner)
c_else = \
                ActionInsertNewClause(end_of_clause_regexp='\}', 
                                      where = 1, direction = 1,
                                      add_lines = 1,
                                      code_bef='\nelse\n{\n\t', code_after='\n}',
                                      back_indent_by=0,
                                      docstring='else clause of a C conditional')

