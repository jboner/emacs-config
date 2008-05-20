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

#
# Code to be added to CmdInterp.
# Used to generate HTML page describing the methods.
#

    def what_can_I_say(self):
        """Generates HTML index of VoiceCode commands (LSAs and CSCs)
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *none* -- 
        """

        index = self.index_cmds_by_topic()
        self.html__cmd_outline(index)
        self.html_cmds_by_topic(index)
        self.html_cmds_alphabetically(index)



    def index_cmds_by_topic(self):
        """Creates an index of LSAs and CSC by language and topic.
        
        **INPUTS**
        
        *none* -- 
        

        **OUTPUTS**
        
        *{STR: {STR: (STR, STR)}* index -- Key is the name of a
         language and value is a dictionary indexing the LSAs and CSCs
         for that language by topic. The key of the later dictionary
         is a topic and the value is a 2ple giving the spoken form of
         the command and a description of its action.
        
        """
        
        index = {}

        #
        # First, index the LSAs
        #
        for a_language in self.language_specific_aliases.keys():
            if a_language == None: a_language = ''
            for an_LSA in self.language_specific_aliases[a_language]:
                spoken, written = insert(sr_interface.spoken_written_form(an_LSA.voc_entry))
                written = re.sub('\n', '\\n', written)
                descr = 'insert \'written\''
                for a_topic in an_LSA.topics:                    
                    self.html_create_index_entry(a_language, a_topic, spoken, descr)

        #
        # Then the CSCs
        #
        for a_CSC in self.cmd_index:
            for spoken in a_CSC.spoken_forms:
                for a_topic in a_CSC.topics:
                    for a_context, an_action in a_CSC.meanings:
                        descr = an_action.doc()
                        try:
                            a_language = a_context.language
                        except:
                            # context is not a language context
                            a_language = None
                        if a_language:
                            self.html_create_index_entry(a_language, a_topic, spoken, descr)

        return index
            




    def html_cmd_outline(self, index):
        """Writes HTML code outlining the list of commands.
        
        **INPUTS**
        
        *{STR: {STR: (STR, STR)}}* index -- Index of commands. See
        [html_index_cmds_by_topic] for details.
        

        **OUTPUTS**
        
        *none* -- 

        .. [html_index_cmds_by_topic] file:///./CmdInterp.CmdInterp.html#html_index_cmds_by_topic"""
        

        print """
<HTML>
<HEADER>
<TITLE>VoiceCode: What can I say?</TITLE>
</HEADER>
<BODY>

<H1>VoiceCode: What can I say?</H1>

<H2>Index</H2>

<UL>"""

        languages = index.keys().sort()
        for a_language in languages:
            
            if a_language == '':
                a_lang_name = 'Global'
            else:
                a_lang_name = a_language

            print '<LI><A HREF="#%s">%s</A>\n   <UL>\n' % (a_lang_name, a_lang_name)
        
            topics = index[a_languages].keys().sort()
            for a_topic in topics:
                url = a_lang_name + '-' + a_topic
                print '      <LI><A HREF="#%s">%s</A>' % (url, a_topic)
            print '   </UL>
        print '</UL>\n<HR>'




    def html_cmds_by_topic(self, index):
        """Prints HTML index of commands by topic
        
        **INPUTS**
        
        *{STR: {STR: (STR, STR)}}* index -- See
        [html_index_cmds_by_topic] for details.
        

        **OUTPUTS**
        
        *none* -- 

        .. [html_index_cmds_by_topic] file:///./CmdInterp.CmdInterp.html#html_index_cmds_by_topic"""
        
        languages = index.keys().sort()
        for a_language in languages:            
            if a_language == '':
                a_lang_name = 'Global'
            else:
                a_lang_name = a_language

            print '<H2><A NAME="%s">%s commands</A></H2>\n\n' % (a_lang_name, a_lang_name)
        
            topics = index[a_language].keys().sort()
            for a_topic in topics:
                url = a_lang_name + '-' + a_topic
                print '<H3><A NAME="%s">%s</A></H3>\n\n' % (url, a_topic)
                for spoken, descr in index[a_language][a_topic]:
                    print '<STRONG>"%s"</STRONG><BR><DD>%s' % (spoken, descr)
        

        print '</BODY>\n</HTML>
