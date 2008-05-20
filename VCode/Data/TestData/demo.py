"""This script demos the capabitilies of VoiceCode.

You can do the demo in gui_sim.py by typing the following command in the
command pane:

execfile('demo.py')

and then using up/down arrows to navigate through the command history.

Make sure that:

- you are in directory 'Data/TestData'
- do File.Open and open 'Data/TestData/large_buff.py' (this makes sure you
 are in the correct directory and also makes sure that symbols for that file
 have been compiled)
- the current buffer is a python one"""


def push_cmd(command):
    """Add a command to the GUI EdSim command history, without executing it"""
    the_mediator.interp.on_app.the_editor.frame.pane.command_prompt.push(command)


#
# Make command history behave like in DOS, i.e. cursor is left at its current
# position when you push a new command onto the history stack.
#
the_mediator.interp.on_app.the_editor.frame.pane.command_prompt.cursor_always_on_top = 0

push_cmd("""# This is a short demo of the capabilities of the VoiceCode programming-by-voice system.""")
push_cmd("""# VoiceCode is a system designed to make it easy to edit source code of programs...""")
push_cmd("""# ... using the NaturallySpeaking speech recognition system.""")
push_cmd("""# Note that VoiceCode is implemented in an editor independant fashion.""")
push_cmd("""# In fact, the simple editor that you see (and which we have build""")
push_cmd("""# for the purpose of this demo) knows nothing about programming-by-voice.""")
push_cmd("""# All the functionality that you are about to see will be usable with any editor...""")
push_cmd("""# ... for which a link to VoiceCode is built.""")
push_cmd("""# Because at the moment VoiceCode doesn't have an error correction dialogue...""")
push_cmd("""# ... I will type commands voice commands into the command line at the bottom of the screen...""")
push_cmd("""# ... instead of voicing them. But saying the commands would work just as well...""")
push_cmd("""# but with about a 20% error rate.""")
push_cmd("""# This error rate will decrease enventually when we are able to correct NatSpeak's errors.""")

push_cmd("""# Let's look at a first example""")
push_cmd("""say(['define', 'method',    'create', 'attribute',    'add', 'argument',    'the', 'attribute',   'method body',    'profile', 'test',    'with argument',    'the', 'attribute'])""")
push_cmd("""# Note that I didn't have to pause between the various commands in this utterance ...""")
push_cmd("""# ... but you can pause if you want at most places where this feels natural.""")
push_cmd("""# Let's decompose that""")
push_cmd("""say(['new stateme', 'back indent'])""")
push_cmd("""say(['define method'])""")
push_cmd("""# Typed template code for method (no need to say 'self' or 'parens') ...""")
push_cmd("""# ... and automatically put cursor at next logical place""")

push_cmd("""say(['create', 'attribute'])""")
push_cmd("""# Didn't have to say underscore for new symbol""")
push_cmd("""say(['add', 'argument'])""")
push_cmd("""# Moved to end of argument list and added comma (wouldn't add comma if argument list empty)""")
push_cmd("""say(['the', 'attribute'])""")
push_cmd("""# Again, didn't have to say "underscore".""")
push_cmd("""say(['method', 'body'])""")
push_cmd("""# Moved to the method body ...""")
push_cmd("""# ... and automatic indentation done by mediator (but editor level auto indent also supported)""")
push_cmd("""say(['profile', 'test'])""")
push_cmd("""# VoiceCode recognised this as a known symbol 'prof_test' """)
push_cmd("""say(['with argument'])""")
push_cmd("""# Types both opening and closing parens and puts cursor in between. """)
push_cmd("""say(['the', 'attribute'])""")
push_cmd("""# Again, didn't have to say "underscore".""")

push_cmd("""# Note that you can dictate non-linearly""")
push_cmd("""# For example, Here's how you dictate balanced expressions""")
push_cmd("""say(['new statement', 'an', 'array',    'at', 'index',    'some', 'function',    'with', 'argument',    'some', 'other', 'function',    'with', 'argument',    '0'])""")
push_cmd("""# VoiceCode types both opening and closing parts of the balanced expressions...""")
push_cmd("""# ... so you will never enter an unbalanced expression in your code again!!!""")
push_cmd("""# Now, I can skip over the closing parts of all the balanced expressions with a single utterance""")
push_cmd("""say(['out', 'of', 'brackets'])""")

push_cmd("""# To navigate, can use many strategies""")

push_cmd("""# Navigation Strategy 1: Navigation by punctuation""")
push_cmd("""say(['previous', 'paren'])""")
push_cmd("""# You can also say if you want to put cursor before or after the punctuation mark""")
push_cmd("""say(['before previous paren'])""")
push_cmd("""# You can also say if you want the previous or the next occurence of the punctuation mark""")
push_cmd("""say(['next', 'bracket'])""")
push_cmd("""say(['previous', 'bracket'])""")
push_cmd("""# Note that searches are repeatable...""")
push_cmd("""say(['again'])""")
push_cmd("""# ... multiple times""")
push_cmd("""say(['3 times'])""")
push_cmd("""# ... and so is any operation of type "repeatable" such as:""")
push_cmd("""# ... page up/down, line up/down, forward/backward char etc.""")
push_cmd("""# In the future, you will be able to repeat such operations in a loop""")
push_cmd("""# For example: 'page up keep doing that' will keep paging up""")
push_cmd("""# ... until you say 'stop'""")
push_cmd("""# Note also that searches are reversible""")
push_cmd("""say(['next one'])""")
push_cmd("""# ... and so is any "bidirectional" operation such as (again):""")
push_cmd("""# ... page up/down, line up/down, forward/backward char etc.""")

push_cmd("""# VoiceCode commands are context sensitive. """)
push_cmd("""# For example, "next one" will repeat the previous command in forward direction...""")
push_cmd("""# ... but only if it is a repeatable bidirectional command.""")
push_cmd("""say(['new statement', 'if', 'not', 'next one', 'then', 'next one', 'equals',    'current', 'one'])""")
push_cmd("""# Note how 'next one' was interpreted as a symbol because""")
push_cmd("""# ... "if" and "then" are not repeatable bidirectional commands""")


push_cmd("""# Navigation Strategy 2: Select Pseudo-Code""")
push_cmd("""# You can use a bit of visible code as a target for moving the cursor to.""")
push_cmd("""# Simply say "Select/Before/After/Previous/Next", and dictate the code you want to move to.""")
push_cmd("""# Note that you have to pause at the beginning and end of such utterances.""")
push_cmd("""# For example, suppose I want to insert a line after the line""")
push_cmd("""#     profile.run("profObject(1000)")""")
push_cmd("""# I could say...""")
push_cmd("""say(['select', 'profObject\\\\profile object', '1000'])""")
push_cmd("""say(['new statement'])""")


push_cmd("""# Note that VoiceCode supports multiple ways of saying the same thing""")
push_cmd("""# For example, to enter template for a new class, you could say:""")
push_cmd("""say(['define class', 'hello', 'world'])""")
push_cmd("""say(['goto body', 'back indent'])""")
push_cmd("""say(['declare class', 'hello', 'world'])""")
push_cmd("""say(['goto body', 'back indent'])""")
push_cmd("""say(['class definition', 'hello', 'world'])""")
push_cmd("""say(['goto body', 'back indent'])""")
push_cmd("""say(['class declaration', 'hello', 'world'])""")
push_cmd("""say(['goto body', 'back indent'])""")
push_cmd("""say(['new class', 'hello', 'world'])""")
push_cmd("""say(['goto body', 'back indent'])""")
push_cmd("""# Or simply: """)
push_cmd("""say(['class', 'hello', 'world'])""")


push_cmd("""# Note also that those same wordings can be used across languages.""")
push_cmd("""# For example, if I open a C++ buffer...""")
push_cmd("""the_mediator.interp.on_app.the_editor.frame.open_file(None)""")
push_cmd("""# ... and say:""")
push_cmd("""say(['class', 'hello', 'world'])""")
push_cmd("""# I get template for a C++ class instead of a Python class""")
push_cmd("""# That's essentially it for the functionality of the current VoiceCode prototype""")
push_cmd("""# The main pieces that are missing for a functional prototype are:""")
push_cmd("""#    - an error correction dialogue (David Fox is implementing a first simple dialogue)""")
push_cmd("""#    - a link to an actual editor (Wieger Wesselink is developping one for jEdit)""")
push_cmd("""# This concludes the demo of VoiceCode.""")



