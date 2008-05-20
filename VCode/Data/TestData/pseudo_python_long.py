import blank space O. S. comma R. E. comma string comma system

import blank space auto test comma natlink comma vc globals
from blank space actions C. C. P. P. import all
from blank space actions python import all
from blank space application state import application state
from blank space context generic import context C. comma ContPy
from blank space context sensitive command import context sensitive command
from blank space ed sim import ed sim
from blank space Object import Object
import blank space ed sim comma symbol dictionary
import blank space S. R. interface

class command interpreter subclass of Object class body
    
define method init with arguments self comma on application equals none comma double asterisk attributes method body

self dot declare attributes with arguments brace pair single quotes untranslated text jump out colon none comma single quotes untranslated text end jump out colon None out of paren

self dot deep construct with arguments command interpreter comma newline brace pair single quotes on application colon on application comma single quotes command index jump out colon empty dictionary comma continue statement

single quotes known symbols jump out colon symbol dictionary dot symbol dictionary without arguments comma continue statement

single quotes cached regular expression jump out colon empty single quotes comma continue statement

single quotes cached regular expression is dirty jump out colon 1 comma continue statement

single quotes language specific aliases jump out colon empty dictionary comma continue statement

single quotes last loaded language jump out colon none jump out continue statement

attributes jump out new line


define method all commands regular expression with arguments self method body

if self dot cached regular expression is dirty if body
self dot cached regular expression equals empty single quotes

all spoken forms equals self dot command index dot keys without arguments

define function compare with arguments x comma y function body
if length with argument x jump out less than length with argument y then return 1
else return minus one /* note define minus one as an LSA cause so frequent */

all spoken forms dot sort with argument compare

          
for a spoken form in list all spoken forms for body

a spoken form equals R. E. dot substitute with arguments single quotes backslash sierra plus sign jump out comma a spoken form

spoken regular expression equals self dot spoken form regular expression with argument a spoken form jump out
                
if self dot cached regular expressionequal to empty single quotes body 

self dot cached regular expression equals spoken regular expression

/* Note: else clause will insert \n before else: */

else clause

self dot cached regular expression equals self dot cached regular expression plus single quotes pipe sign plus spoken regular expression

return self dot cached regular expression

/* rendu ici 12/10/00 */

    def spoken formregular expression(self comma spoken form):
        """Returns aregular expression that matches a spoken form of a command dot 

        *STR spoken form* is the spoken form dot  The returnedregular expression will match
        it even if the case of the first letter of each word do not match dot """

        words equals re dot split('\s+' comma spoken form)
       regular expression equals ''
        for aword in words:
            first equals aword[0]
            rest equals aword[1:]
           regular expression this word equals '[' + string dot lower(first) + string dot upper(first) + ']' + rest
            if notregular expressionequal to '':
               regular expression =regular expression + '\s*'
           regular expression =regular expression +regular expression this word
        returnregular expression

    def interpret NL cmd(self comma cmd):
        
        """Interprets a natural language command and executes
        corresponding instructions dot 

        *STR cmd* is the spoken form of the command dot 
        """
        
#        print '-- command interpreter dot interpret NL cmd: cmd=%s' % cmd
#        print '-- command interpreter dot interpret NL cmd: self dot all commandsregular expression()=%s' % self dot all commandsregular expression()

        self dot  untranslated text start equals None
        self dot  untranslated text end equals None
        
       regular expression equals '^(\s*)(' + self dot all commandsregular expression() + ')(\s*)'

        #
        # Process the beginning of the command until there is nothing
        # left
        #
        while (not cmdequal to ''):
#             print '-- command interpreter dot interpret NL cmd: now comma cmd=%s' % cmd
#            print '-- command interpreter dot interpret NL cmd: cur pos=%s' % self dot on application dot curr buffer dot cur pos
#             self dot on application dot print buff content()

             #
             # Check for a CSC at the beginning of the command comma and compute
             # length of string it consumes
             #
             csc consumes equals 0
             csc match equals re dot match(regexp comma cmd)
             if (csc match):
                 csc consumes equals csc match dot end(2) - csc match dot start(2) + 1
                 cmd without csc equals cmd[csc match dot end():]                 

             #
             # Check if command starts with a known symbol comma and compute
             # length of string it consumes
             #
             (a symbol comma cmd without symbol) equals self dot chop symbol(cmd)
             symbol consumes equals len(cmd) - len(cmd without symbol)

#            print '-- command interpreter dot interpret NL cmd: csc consumes=%s comma symbol consumes=%s' % (csc consumes comma symbol consumes)

             #
             # Translate either CSC or known symbol comma depending on which
             # of the two consumes the longest part of the NL command
             #
             if csc consumes and csc consumes >= symbol consumes:
                 #
                 # The CSC consumes more than the symbol comma so translate it dot 
                 # Try every possible contexts until one applies
                 #                
                 orig spoken form equals csc match dot group(2)
#                print '-- command interpreter dot interpret NL cmd: matched spoken form \'%s\'' % orig spoken form                                
                 indexed spoken form equals orig spoken form
                 re dot sub('\s+' comma ' ' comma indexed spoken form)
                 CSCs equals self dot command index[string dot lower(indexed spoken form)]
                 csc applied equals 0                 
                 for aCSC in CSCs:
                     csc applied equals aCSC dot interpret(self dot on application)
                     if (csc applied):
                         break
                 if csc applied:
                     # Found applicable context for the CSC
#                    print '-- command interpreter dot interpret NL cmd: csc applied CSC \'%s\'' % indexed spoken form
                     cmd equals cmd without csc
                 else:
                     #
                     # Found no applicable contexts so CSC couldn't consume
                     # anything after all
                     #
                     csc consumes equals 0
                         
             if symbol consumes and symbol consumes >= csc consumes:
                #
                # Command doesn't start with CSC comma or CSC consumes less than
                # the symbol dot 
                #
                # So comma insert the symbol
                #
                # Note: known symbols are inserted as untranslated
                #       text because often comma the user will create new symbols
                #       by prefixing/postfixing existing ones dot  For example,
                #       if you define a subclass of a known class SomeClass
                #       you may name the new class SomeprefixSomeClass or
                #       SomeClassSomepostfix dot 
                #
#                print '-- command interpreter dot interpret NL cmd: inserted symbol %s' % a symbol                                
                self dot insert untranslated text(a symbol)
                cmd equals cmd without symbol
                
             if not csc consumes and not symbol consumes:
                #
                # Command starts with neither CSC or symbol dot 
                # Just remove a word from the beginning of the
                # command and insert it into the application's buffer
                #
#                print '-- command interpreter dot interpret NL cmd: inserted first word as is'
                amatch equals re dot match('(^\s*[^\s]*)' comma cmd)
                leading word equals amatch dot group(1)
                self dot insert untranslated text(leading word)
                cmd equals cmd[amatch dot end():]

             if (csc consumes or cmdequal to '') and \
                self dot  untranslated text start != None:
                #
                # A CSC was translated comma or we reached end of the
                # command comma thus marking the end of a sequence of untranslated
                # text dot  Try to match it to a known (or new) symbol dot 
                #
                self dot match untranslated text()

#             print '-- command interpreter dot interpret NL cmd: End of while dot  self dot  untranslated text start=%s comma self dot  untranslated text end=%s comma self dot on application dot curr buffer dot cur pos=%s' % (self dot  untranslated text start comma self dot  untranslated text end comma self dot on application dot curr buffer dot cur pos)
                                
    def insert untranslated text(self comma text):
        
        """Inserts some text in current buffer comma and marks it as untranslated
        text dot 

        The next time a CSC is encountered comma the interpreter will try
        to match that text (and all untranslated text that immediatly
        precedes/follows it) to a new symbol comma or a known symbol with
        unresolved spoken forms dot 
        
        **INPUTS**
        
        *ST* text -- The text to be inserted
        

        **OUTPUTS**
        
        *none* -- 
        """
        if self dot  untranslated text startequal to None:
            #
            # This is the beginning of a sequence of
            # untranslated text dot  Remember its start
            # position dot 
            #
            self dot  untranslated text start equals self dot on application dot curr buffer dot cur pos

        self dot on application dot insert indent(text comma '')
        
        #
        # Remember end of the current sequence of untranslated
        # text dot 
        #
        self dot  untranslated text end equals self dot on application dot curr buffer dot cur pos

#        self dot on application dot print buff content()
#        print '-- command interpreter dot insert untranslated text: self dot  untranslated text start=%s comma self dot  untranslated text end=%s comma untranslated region=\'%s\'' % (self dot  untranslated text start comma self dot  untranslated text end comma self dot on application dot curr buffer dot content[self dot  untranslated text start:self dot  untranslated text end])
        

    def match untranslated text(self):
        """Tries to match last sequence of untranslated text to a symbol dot 
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

#        print '-- command interpreter dot match untranslated text: self dot  untranslated text start=%s comma self dot  untranslated text end=%s comma untranslated region=\'%s\'' % (self dot  untranslated text start comma self dot  untranslated text end comma self dot on application dot curr buffer dot content[self dot  untranslated text start:self dot  untranslated text end])
        
        untranslated text equals self dot on application dot curr buffer dot content[self dot  untranslated text start:self dot  untranslated text end]

        text no spaces equals re dot match('\s*([\s\S]*)\s*$' comma untranslated text) dot group(1)
        #
        # Don't bother the user if the untranslated text is just a known symbol
        #
        if not self dot known symbols dot symbol info dot has key(text no spaces):
#        print '-- command interpreter dot match untranslated text: trying to match untranslated text to a symbol dot  untranslated text=\'%s\' comma self dot  untranslated text start=%s comma self dot  untranslated text end=%s' % (untranslated text comma self dot  untranslated text start comma self dot  untranslated text end)
            symbol matches equals self dot known symbols dot match pseudo symbol(untranslated text)
            if symbol matches:
                self dot dlg select symbol match(symbol matches)

        #
        # There is no more untranslated region
        #
        self dot  untranslated text start equals None
        self dot  untranslated text end equals None
        

    def dlg select symbol match(self comma symbol matches):
        """Asks the user to select a match for pseudo symbol dot 
        
        **INPUTS**
        
        *[SymbolMatch]* symbol matches -- List of possible matches dot 
        

        **OUTPUTS**
        
        *none* -- 

         dot  dot  [SymbolMatch] file:/// dot /symbol dictionary dot SymbolMatch dot html"""

        untranslated text equals self dot on application dot curr buffer dot content[self dot  untranslated text start:self dot  untranslated text end]

        good answer equals 0
        while not good answer:
            print 'Associate \'%s\' with symbol (Enter selection):\n' % untranslated text
            print '  \'0\': no association'
            ii equals 1
            for a match in symbol matches:
                sys dot stdout dot write('  \'%s\': %s' % (ii comma a match dot native symbol))
                if a match dot is new:
                    sys dot stdout dot write(' (*new*)')
                sys dot stdout dot write('\n')
                ii equals ii + 1
#                print '-- command interpreter dot dlg select symbol match: a match dot score()=%s comma a match dot   dict  =%s' % (a match dot score() comma a match dot   dict  )
            sys dot stdout dot write('\n> ')
            answer equals sys dot stdin dot readline()
            answer match equals re dot match('\s*([\d])+\s*' comma answer)
            if answer match:
                choice index equals int(answer match dot group(1)) - 1
                if choice index < len(symbol matches) and choice index >= -1:

                    good answer equals 1
            if not good answer:
                print 'Invalid answer \'%s\'' % answer

        #
        # Accept the match
        #
        if choice index >= 0:
            #
            # A match was chosen dot  Accept it and type it instead of the
            # untranslated text dot 
            #
            chosen match equals symbol matches[choice index]
            self dot known symbols dot accept symbol match(chosen match)

            #
            # Insert matched symbol comma then go back to where we were
            #
            old pos equals self dot on application dot curr buffer dot cur pos
            self dot on application dot insert indent(chosen match dot native symbol comma '' comma start=self dot  untranslated text start comma end=self dot  untranslated text end)
            new pos equals old pos + len(chosen match dot native symbol) - (self dot  untranslated text end - self dot  untranslated text start)
            self dot on application dot move to(new pos)
            
        
        #
        # Now comma there is no more untranslated text dot 
        #
        self dot  untranslated text start equals None
        self dot  untranslated text end equals None
            

    def chop symbol(self comma command):
        """Chops off the beginning of a string if it matches a known symbol dot 

        If more than one symbols are possible comma returns the symbol that
        consumes the greateest number of words from command dot 
        
        **INPUTS**
        
        *STR* command -- the string from which we want to chop off a symbol dot 
        

        **OUTPUTS**

        Returns a pair *(best symbol comma rest)* where:
        
        *STR* best symbol -- is the symbol that was chopped off (in native
         format) dot  If *None* comma it means *command* did not start with a
         symbol dot 

        *STR* rest -- is what was left of *command* after the symbol
         was chopped off dot 
        
        """

#        print '-- command interpreter dot chop symbols: command=%s' % command

        (best symbol comma rest) equals (None comma command)

        #
        # Split the command into words
        #
        command equals re dot sub('(\W+)' comma ' \\1 ' comma command)
        command equals re dot sub('(^\s+|\s+$)' comma '' comma command)
        words equals re dot split('\s+' comma command)                
        upto equals len(words)

        #
        # Starting with the whole command and dropping words from the end,
        # check if that corresponds to a known symbol dot 
        #
        while upto:
            a spoken form equals string dot join(words[0:upto] comma ' ')
            a spoken form equals string dot lower(a spoken form)
#            print '-- command interpreter dot chop symbols: upto=%s comma a spoken form=%s' % (upto comma a spoken form)
            if self dot known symbols dot spoken form info dot has key(a spoken form):
                # This corresponds to the spoken form of a symbol
#                print '-- command interpreter dot chop symbols: matches a known symbol'
                best symbol equals self dot choose best symbol(a spoken form comma self dot known symbols dot spoken form info[a spoken form] dot symbols)
                words equals words[upto:]
                rest equals string dot join(words comma ' ')
                break
            upto equals upto - 1
        return (best symbol comma rest)
        
        


    def choose best symbol(self comma spoken form comma choices):
        """Chooses the best match for a spoken form of a symbol dot 

        For now comma we just choose the first item in *choices* comma but in
        the future comma we might choose the one that appears closest to
        the cursor comma or the one that used most recently comma or the one
        that best matches the spoken form dot 
        
        **INPUTS**
        
        *STR* spoken form -- spoken form of the symbol dot  
        
        *ANY* choices -- undocumented 
        

        **OUTPUTS**
        
        *none* -- 
        """

        return choices[0]

    def index csc(self comma acmd comma add voc entry=1):
        """Add a new csc to the command interpreter's command dictionary

        [context sensitive command] *acmd* is the command to be indexed dot 

        *BOOL add voc entry equals 1* -- if true comma add a SR vocabulary entry
         for the CSC's spoken forms

         dot  dot  [context sensitive command] file:/// dot /context sensitive command dot context sensitive command dot html"""

        globalregular expression is dirty

       regular expression is dirty equals 1

        for a spoken form in acmd dot spoken forms:
            #
            # Remove leading comma trailing and double blanks from the spoken form
            #
            re dot sub('\s+' comma ' ' comma a spoken form)
            re dot sub('^\s+' comma '' comma a spoken form)
            re dot sub('\s+$' comma '' comma a spoken form)
            a spoken form equals string dot lower(a spoken form)

            #
            # Index the spoken form
            #
            if (self dot command index dot has key(a spoken form)):
                #
                # Already indexed dot  Just add to the list of CSCs for that
                # spoken form
                #
                commands this spoken form equals self dot command index[a spoken form]
                commands this spoken form[len(commands this spoken form):] equals [acmd]
            else:
                #
                # First time indexed dot  Create a new list of CSCs for that
                # spoken form comma and add it to the SR vocabulary dot 
                #
                self dot command index[a spoken form] equals [acmd]
                if not os dot environ dot has key('VCODE NOSPEECH') and add voc entry:
                    S dot  R dot  interface dot addWord(a spoken form)



    def load language specific aliases(self):
        
        """Loads words specific to the language of the current buffer,
        if needed dot 

        Also comma unloads words specific to previously loaded language if needed dot 
        
        **INPUTS**
        
        *none*

        **OUTPUTS**
        
        *none* -- 
        """

        language equals self dot on application dot curr buffer dot language
        last language equals self dot last loaded language
#        print '-- command interpreter dot load language specific aliases: called comma language=%s comma last language=%s' % (language comma last language)
#        print '-- command interpreter dot load language specific aliases: self dot language specific aliases[%s]=%s' % (language comma self dot language specific aliases[language])
        if language != last language:
            #
            # Remove words from previous language
            #
            if self dot language specific aliases dot has key(last language):
                for a word in self dot language specific aliases[last language]:
                    S dot  R dot  interface dot deleteWord(a word)
            
            #
            # Add words for new language
            #
            if self dot language specific aliases dot has key(language):
                for a word in self dot language specific aliases[language]:
                    S dot  R dot  interface dot addWord(a word)

            self dot last loaded language equals language
            
#        print '-- command interpreter dot load language specific aliases: finished'
