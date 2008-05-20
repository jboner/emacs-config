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
from HTMLgen import *
from debug import trace
import vc_globals
import pprint
import webbrowser
import re, os
from copy import copy
from cont_gen import *
from Context import scope_order, valid_scope
from CSCmd import CSCmdList
import util
from utilsqh import normaliseaccentedchars
try:
    set
except NameError:
    from sets import Set as set

number_of_columns = 3

class WhatCanISay(object):
    """
    A class for generating, and displaying an index of all the active
    voice commands.

    Written mainly by Quintijn Hoogenboom, Dec 2006 -- Febr 2007

    Collects all commands (LSA and CSC (later)), and puts them in a website.

    The website is placed on VCODE_HOME/Data/WhatCanISay.


    
    """
    def __init__(self):
        # for overview page:
        self.number_of_columns = number_of_columns
        self.index = {}      # non trivial index
        self.boilerplate = {}   # irrespective of language (lsa)
        self.all_lang = None
        self.curr_context = None
        self.boilerplate_groups = ('standard grouping (US English) navigation',
                                   'standard punctuation (US English) navigation',
                                   'standard grouping (US English)',
                                   'standard punctuation (US English)',
                                   'standard punctuation',
                                   'standard punctuation navigation',
                                   'alternate grouping navigation',
                                   'alternative punctuation navigation',
                                   'escaped characters',
                                   'military letters',
                                   'US small numbers',
                                   'between standard grouping (US English)',
                                   'between standard quotes (US English)',
                                    'between alternate grouping',
                                    'between alternate quotes',
                                    'alternate grouping',
                                    'alternate quotes',
                                    'alternate quotes navigation',
                                    'alternate punctuation',
                                    'standard quotes (US English)',
                                    'standard quotes (US English) navigation',
                                    'alternative punctuation',
                                   
                                   )
                                   
    def load_commands_from_interpreter(self, app, interp, curr_lang, curr_context=None, all_lang=None):
        """Get a dictionary of all commands from the interpreter

        enter the curr_lang
        default is only current language, with all contexts

        as extension the current context can be taken (yo what can i say now), or
        all_languages can be taken (yo what can I say complete)

        The list of languages goes in self.languages

        The LSA commands are put in self.lsa_index, a dict
         with keys None and all the possible languages.
         and values:  a list of tuples (wordList, LSAentry)

        The CSC commands are put in self.index.
         with keys: the languages
         with values: a dict with keys 'spoken form'
                             and values: [(context key, action),...]

        """
        self.index = {}
        self.boilerplate = {}
        self.app = app
        self.cmd_interp = interp   # needed only for yo what can I say now (so curr_context = 1)
        self.all_lang = all_lang
        self.curr_context = curr_context
        self.curr_lang = curr_lang
        if self.all_lang and self.curr_context:
            raise ValueError('WhatCanISay cannot be called with both "all_lang" and "curr_context" set')
        if not self.curr_lang in all_languages:
            raise ValueError('WhatCanISay cannot be called with both "all_lang" and "curr_context" set')
            
        self.create_html_folder()            
        self.languages = self.cmd_interp.supported_languages()
##        self.languages.sort()
        if all_lang:
            self.languages = self.languages
        else:
            self.languages = (curr_lang, )
        if curr_lang not in self.languages:
            raise ValueError('Cannot do WhatCanISay for language %s, because there are no commands for this language'% curr_lang)
        
        self.curr_context = curr_context
        self.index_cscs()
        self.index_lsas()
        # per language (keys):
        # values dict with spoken_form as keys, values a list of
        # string (lsa) or list (csc, info form of context, action combinations)

    def index_lsas(self):
        cmd_interp = self.cmd_interp
        for a_language in self.languages:
            if not (self.all_lang or a_language == self.curr_lang):
                continue
            self.index.setdefault(a_language, {})
            curr_index = self.index[a_language]
            wTrie = cmd_interp.language_specific_aliases[a_language]
            for an_LSA in wTrie.items():
                wordList, entry = an_LSA
                # get the info dict of an aliasmeaning:
                info = entry.get_info()
                spoken_form_text = ' '.join(wordList)

                set_name = info['setname']
                if  set_name in self.boilerplate_groups and not self.all_lang:
                    continue   # Common commands only if all_lang is asked for
##                     if spoken_form_text not in self.boilerplate:
##                         # Assume here this lsa (spoken_form) in all_languages, maybe should be tested
##                         self.boilerplate.setdefault(spoken_form_text, []).append(info)
                if self.curr_context and spoken_form_text in curr_index:
                    # some csc already applies obviously (probably)
                    continue
                curr_index.setdefault(spoken_form_text, []).append(info)

    def index_cscs(self):
        cmd_interp = self.cmd_interp
        for a_language in self.languages:
            self.index.setdefault(a_language, {})

        wTrie = cmd_interp.commands
        for a_csc_entry in wTrie.items():
            the_spoken_form = ' '.join(a_csc_entry[0])
            the_meanings = a_csc_entry[1]
##            trace('WhatCanISay.index_cscs', "** Processing a_csc_entry: the_spoken_form: %s, the_meanings: %s" % \
##                  (the_spoken_form, the_meanings))
            self.index_contextual_meanings(the_spoken_form, the_meanings)

    def info_is_csc(self, info):
        """info is a list"""
        return isinstance(info, list)
    def info_is_lsa(self, info):
        """info is a dict"""
        return isinstance(info, dict)

    def index_contextual_meanings(self, spoken_form, meanings_list):
        
        for a_language in self.languages:
            if self.curr_context:
                for_this_language = meanings_list.applies(self.app, preceding_symbol=None)
            else:
                for_this_language = CSCmdList()
                for a_context, an_action, a_ref in meanings_list:
##                    trace('WhatCanISay.index_contextual_meanings', "** Processing a_context=%s, an_action: %s" % \
##                     (a_context.equivalence_key(), an_action.doc() or "action???"))
                    if self.context_applies_for_lang(a_language, a_context):
                        for_this_language.append((a_context, an_action, a_ref))

            # now get the info of the CSCmdList:
            if for_this_language:
                info = for_this_language.get_info()
                setnames = list(set([i['setname'] for i in info]))
                if len(setnames) > 1:
                    print 'WhatCanISay, index_contextual_meanings: duplicate setnames for command: %s: %s'%\
                          (spoken_form, setnames)
                    setnames.sort()
                setname = setnames[0]
                if setname in self.boilerplate_groups and not self.all_lang:
##                        trace('WhatCanISay.index_contextual_meanings', "** boilerplate: %s"% \
##                              set_name)
                        continue      # Common commands only if all_lang is asked for
                self.index[a_language].setdefault(spoken_form, []).append(info)
                
                 
           
    def context_applies_for_lang(self, language, context):
        trace('WhatCanISay.context_applies_for_lang', "** language=%s, context=%s" %(language, context))
        answer = False        
        if isinstance(context, ContLanguage) and \
            language in context.language:
           answer = True
        elif isinstance(context, ContAny):
           answer = True
        return answer

    def create_cmds(self):
##        self.create_html_folder()   # only for test, remove afterwards        
##        print 'In future make wcisay website for lang: %s, curr_context: %s, all_lang: %s'% \
##              (self.curr_lang, self.curr_context, self.all_lang)
##        all = pprint.pformat(self.index)
##        if self.curr_context:
##            outfile = os.path.join(self.html_folder, "%s_curr_context.txt"% self.curr_lang)
##        elif self.all_lang:
##            outfile = os.path.join(self.html_folder, "all_lang.txt")
##        else:
##            outfile = os.path.join(self.html_folder, "%s_default.txt"% self.curr_lang)

## only when doing the webpages from the _run command of WhatCanISayTest::::
##        total = "self.index = %s\n"% all        
##        execfile(outfile)
##        print 'collecting from %s'% outfile
##        open(outfile, "w").write(total)
##        print "contents of WCISay written to: %s"% util.within_VCode(outfile)
        self.top_menu = self.create_top_menu()
        if not self.top_menu:
            raise ValueError("no language(s) present in WhatCanISay index")
        self.top_menu_keys = self.top_menu.keys()
        self.top_menu_keys.sort()
        self.left_menu = {}
        self.left_menu_keys = {}
        # collect data:
        for top in self.top_menu_keys:
            self.part_index = self.index[top]
            self.left_menu[top] = self.create_left_menu(top=top.lower())  # make all pages lowercase
            keys = self.left_menu[top].keys()
            keys.sort()
            self.left_menu_keys[top] = keys

    def create_html_pages(self):
        """now on to the html"""
        self.create_html_folder()
        global_page = self.html_command_index()
        for top in self.top_menu_keys:
            self.anchors = {}  # make anchors per language
            specific_page = self.html_overview_page(page=self.top_menu[top], part=top)
            left_menu = self.left_menu[top]
            left_keys = left_menu.keys()
            left_keys.sort()
            for left in left_keys:
                self.html_detail_page(page=left_menu[left], part=top, detail=left)
        # index_page needed in the next step:
        if self.all_lang:
            self.index_page =  global_page
        else:
            self.index_page =  specific_page

    def create_top_menu(self):
        """make a dict of all languages that come in the top"""
        m = {}
        for k in self.index.keys():
            m[k] = '%s_overview.html'% k.lower()
        return m

    def create_left_menu(self, top):
        """make a dict of all subjects of the (language dependent) index"""
        m = {}
        for entry in self.part_index.keys():
            result = self.part_index[entry]
            setnames = self.get_setnames(result)  # this is a list of lsa or csc defs
                                             # so a meanings list...
            for s in setnames:
                self.add_to_dict(m, s, top)
        return m

    def add_to_dict(self, Dict, name, top):
        """ make entry to a dictionary of left menu

        keys are setname (without spaces, values are the pages
        """
        if name in Dict:
            return
        output_name = normaliseaccentedchars(name).lower()
        Dict[name] = '%s_%s.html'% (top,output_name)
##        print 'html page name: %s'% Dict[name]
        

    
    def get_written_form_from_action(self, action):
        try:
            doc = action.doc()
        except AttributeError:
            doc = 'no doc for %s'% repr(action).split()[0][1:]
        return doc


    def bring_to_top(self, List, item):
        """Change inplace the list"""
        if item in List:
            List.remove(item)
            List.insert(0, item)

    def show_cmds(self):
        """show the commands after the previous steps...

        """
        webbrowser.open_new(self.index_page)

    def create_html_folder(self):
        """create a folder for the WhatCanISay website

        this foldername is also returned
        """
        self.html_folder = vc_globals.wcisay_html_folder

        try:
            os.makedirs(self.html_folder)
            print 'WARNING: whatCanISay folder did not exist, stylesheet will not be available'
        except:
            pass
        if not os.path.isdir(self.html_folder):
            raise Exception('not a valid directory for What Can I Say website: %s'% \
                            util.within_VCode(self.html_folder))
        return self.html_folder
    
    
    def html_command_index(self):
        """make the index page of the website"""
        doc = SimpleDocument()
        doc.stylesheet = "vc.css"
        page = 'index.html'
        page_type = 'index'
        page_html = 'index.html'
        doc.append(self.html_header(page, part=page_type, detail='home'))
        tlpage = FullTable(Class="page")
        trpage = TR()
        tdpage = TD(Class="body")
        # produce the menu (left):
        # produce the body:
        VcodeWebsite = Href("http://voicecode.iit.nrc.ca/VoiceCode/public/ywiki.cgi", "Voice Code website", target="_blank")

        text =['This is the What Can I Say (actual) information of your VoiceCode instance.', '',
               'By default (with "yo what can I say") you get information for the current programming language, excluding common commands like punctuation and navigation on punctuation.', '',
                'If you call "yo what can I say now" only commands that apply at the moment you call this command will be shown.<br>(Note: this information does not go through exactly the same routine, so could differ from the real information.)', ''


'If you call "yo what can I say all" all information is shown, including "common" commands, and for all languages available.',
                '', '',
                '', '',
                'For the general information please consult the '+VcodeWebsite+'.',
                '',
                '']        
        for t in text:
            if t:
                tdpage.append(Paragraph(t))
            else:
                tdpage.append(Paragraph("&nbsp;", Class="blank"))
        doc.append(tlpage(trpage(tdpage)))
        doc.append(self.html_footer(page, part=page_type, nice_name='home'))
        outfile = os.path.join(self.html_folder, 'index.html')
   
        trace('WhatCanISay.files', 'making page: %s'% util.within_VCode(outfile))
        doc.write(outfile)
        return outfile

    def get_left_menu(self, me, part):
        """produce a menu with me without link

        me = name of calling page, page_type (part) is to be included before the link name

        """
        td = TD(Class="leftmenu")
        pages = self.left_menu[part]
        pages_keys = self.left_menu_keys[part]
        for p in pages_keys:
            niceP = p
            if pages[p] == me:
                td.append(Paragraph(niceP, Class="lefton"))
            else:
                to_page = pages[p]
                td.append(Paragraph(Href(to_page, niceP), Class="leftoff"))
        return td

    def html_overview_page(self, page, part):
        """generate a overview page of all commands in a language"""
        doc = SimpleDocument()
        doc.stylesheet = "vc.css"

        index = self.index[part]
        names = index.keys()
        names.sort()
        left_menu = self.left_menu[part]
        page_html = page
        
        content = names
        
        doc.append(self.html_header(page, part, 'overview'))
        tlpage = FullTable(Class="page")
        trpage = TR()
        # produce the menu (left):
        leftMenu = self.get_left_menu(page, part)
        trpage.append(leftMenu)

        # now the contents:        
        tl = FullTable(Class="body")
        tr = TR()
        # define in top, possibly configure in vc_config or user_config.
        per_col = self.number_of_columns
        tdspacer = TD("&nbsp;", Class="spacer")
        rows = len(content)/per_col
        if len(content)%per_col:
            rows += 1
        cell_num = 0
        
        for start in range(rows):
            cell_num = start % 2
            for col in range(start, len(content), rows):
                cell_num += 1
                k = content[col]
                setnames = self.get_setnames(index[k])
                links = []
                nice_names = []
                for s in setnames:
                    if s in left_menu:
                        links.append(left_menu[s])
                        nice_names.append(s)
                    else:
                        raise ValueError("WhatCanISay, setname not in left menu: %s (key: %s)"%
                                         (s, k))
                if not links:
                    tr.append(TD(k, Class="written%s"% (cell_num%2,)))
                elif len(links) == 1:
                    anchor = self.get_anchor(k)
                    link = '%s#%s'% (links[0], anchor)
                    link = Href(link, k)
                    tr.append(TD(link, Class="written%s"% (cell_num%2,)))
                else:
                    to = []
                    for link, nice in zip(links, nice_names):
                        # more entries!!!!!
                        anchor = self.get_anchor(k)
                        the_link = '%s#%s'% (link, anchor)
                        text = "%s (%s)"% (k, nice)
                        link = Href(link, text)
                        to.append(str(link))
                    tr.append(TD('<br>'.join(to), Class="written%s"% (cell_num%2,)))
                        
                tr.append(tdspacer())
            tl.append(tr)
            tr.empty()
        
        trpage.append(TD(tl, Class="body"))
        tlpage.append(trpage)
        doc.append(tlpage)
        doc.append(self.html_footer(page, part=part, nice_name=part))
        outfile = os.path.join(self.html_folder, page_html)
        trace('WhatCanISay.files', 'making page: %s'% util.within_VCode(outfile))
        doc.write(outfile)
        return outfile

    def has_setname(self, list_of_meanings, name):
        """see if name is present in the meanings dict"""
        for item in list_of_meanings:
            if self.info_is_lsa(item):
                # lsa:
                setname = item.get('setname', 'xxx')
                if setname == name: return 1
            elif self.info_is_csc(item):
                for inner_item in item:
                    # csc, do all parts:
                    setname = inner_item.get('setname', 'xxx')
                    if setname == name: return 1            
            else:
                raise ValueError('invalid type for WhatCanISay entry for %s: %s'% 
                                 (entry, `item`)  )      
        # fall through: false

                                 
    def get_setnames(self, list_of_meanings):
        """get the setnames from a meanings dict

        note: returns a list (with possibly 'unnamed' in it!
        """
        S = set()
        for item in list_of_meanings:
            if self.info_is_lsa(item):
                # lsa:
                setname = item.get('setname', 'unnamed')
                S.add(setname)
            elif self.info_is_csc(item):
                for inner_item in item:
                    # csc, do all parts:
                    setname = inner_item.get('setname', 'unnamed')
                    S.add(setname)
            
            else:
                raise ValueError('invalid type for WhatCanISay entry for %s: %s'% 
                                 (entry, `item`))
        return list(S)
                  
    def html_detail_page(self, page, part, detail):
        """generate a detail page on one setname"""
        
        doc = SimpleDocument()
        doc.stylesheet = "vc.css"
        page_type = part
        page_html = page


        
        trace('WhatCanISay.html_detail_page', 'page: %s, part: %s, detail: %s'%
              (page, part, detail))

        content = self.index[part]
        keys = [k for (k, v) in content.items() if self.has_setname(v, detail)]
        keys.sort()
        trace('WhatCanISay.html_detail_page', 'keys:  %s'% keys)
        doc.append(self.html_header(page, part=page_type, detail=detail))

        if 1:
            tlpage = FullTable(Class="page")
##            doc.append(Header(1, 'begin of csc commands, python'))
            trpage = TR()

            # produce the menu (left):
            leftMenu = self.get_left_menu(page, part)
            trpage.append(leftMenu)
            
            # now the contents:        
            tl = FullTable(Class="body")
            tr = TR()
            cell_num = 0
            tdspacer = TD("&nbsp;", Class="spacer")
            all_meanings = {}  #of tuples 1: list of keys
                               #          2: the item
            # collect in all_meanings:            
            for k in keys:
                the_key = ''
                item = content[k]
                for meaning in item:
                    if self.info_is_csc(meaning):
                        m = meaning[0]
                        cont = m['equiv']
                        scope = m['scope']
                        action = m['action']
                    elif self.info_is_lsa(meaning):
                        cont = 'lsa'
                        scope = ''
                        action = meaning.get('written_form','')
                    the_key += action + scope + cont
                if the_key in all_meanings:
                    all_meanings[the_key][0].append(k)
                else:
                    all_meanings[the_key] = ([k], item)


            meaning_keys = all_meanings.keys()
            meaning_keys.sort()
            for k in meaning_keys:
                Keys, Meanings = all_meanings[k]
                
                cell_num += 1
                class_name = "written%s"% (cell_num%2,)
                
                trmeanings = []
                rows_inside = 0
                meaning_num = 0
                for meaning in Meanings:
                    if self.info_is_csc(meaning):
                        for m in meaning:
                            meaning_num += 1
                            class_name_meanings = "%s%s"% (class_name, meaning_num%2)
                            cont = m['equiv']
                            scope = m['scope']
                            action = m['action']
                            trmeanings.append(self.fill_row(cont, scope, action, class_name_meanings))
                            rows_inside += 1
                    elif self.info_is_lsa(meaning):
                        meaning_num += 1
                        class_name_meanings = "%s%s"% (class_name, meaning_num%2)
                        cont = 'lsa'
                        scope = ''
                        action = meaning.get('written_form','')
                        trmeanings.append(self.fill_row(cont, scope, action, class_name_meanings))
                        rows_inside += 1

                    else:
                        raise ValueError("WhatCanISay, invalid meaning at key: %s (%s)"%
                                         (Keys, `meaning`))
                key_part = []
                Keys.sort()
                for spoken in Keys:
                    anchorname = self.get_anchor(spoken)
                    anchor = '<a name="%s">'% anchorname
                    spoken = spoken.replace(' ', "&nbsp;")
                    key_part.append(anchor+spoken+"</a>")
                trmeanings[0].prepend(TD('<br>'.join(key_part), Class=class_name, rowspan=rows_inside))
                for tr in trmeanings:
                    tl.append(tr)
        
            trpage.append(TD(tl, Class="body"))
            tlpage.append(trpage)
            doc.append(tlpage)
            doc.append(self.html_footer(page, part=page_type, nice_name=detail))
        outfile = os.path.join(self.html_folder, page_html)
        trace('WhatCanISay.files', 'making page: %s'% util.within_VCode(outfile))
        doc.write(outfile)

    def fill_row(self, cont, scope, action, class_name):
        tr = TR()
        tr.append(TD(action, Class=class_name))
        tr.append(TD(scope, Class=class_name))
        tr.append(TD(cont, Class=class_name))
        return tr

    def get_anchor(self, anchor):
        """in order to prevent duplicate anchors in a page
        get from self.anchors

        """
        if anchor in self.anchors:
            return self.anchors[anchor]
        
        i = 0
        squeezed = normaliseaccentedchars(anchor).lower()  # remove spaces etc
        anchor_values = self.anchors.values()
        if squeezed not in self.anchors.values():
            self.anchors[anchor] = squeezed
            return squeezed
        while 1:
            i += 1
            new_squeezed = "%s%s"% (squeezed, i)
            if new_squeezed not in anchor_values:
                self.anchors[anchor] = new_squeezed
                return new_squeezed
                
            
                  
    def html_header(self, page, part, detail):
        """produce the header at the top of the html page, with the name mentioned

        page_type can be 'index', 'lsa', 'csc'

        """
        page_type = part
        page_html = page
        # assume height of picture = 90, h2 is height of top_menu
        h_pic = 90
        h2 = 28
        h1 = h_pic - h2 + 20 # allow 20 for MLB number space in firefox
        # assume w of picture = 134
        w = 134
        im = Img('vcodeuser.jpg', alt='what can i say website for voicecoder')
        L = []
##        print 'header of page: %s, page_type: %s'% (page, page_type)
        if page_type == 'index' and page == 'index.html':
            pass
        else:
            im = Href('index.html', im)

        if page_type != 'index':
            niceP = '%s'% detail
        else:
            niceP = 'home'
        if self.all_lang:
            niceP += '<br>(all languages, including common commands)'
        elif self.curr_context:
            niceP += '<br>(only commands that are valid in the current context, but excluding common commands)'
            
            
        text = "VoiceCoder What Can I Say (%s): %s"% (self.curr_lang, niceP)
        tl = FullTable(Class="header")
        tl_menu = FullTable(Class="header", height=h2)
        tl.append(TR(TD(im, Class="bannerim", rowspan=2, width=w, height=h_pic),
                     TD(text, Class="bannertext", height=h1)))
        # make the top menu:
        tr = TR()
        keys = ['home'] + self.top_menu_keys
        for menu in keys:
            start_page = self.get_first_page(menu)
#            print 'menu: %s, start_page: %s, page: %s, page_html: %s'% \
#                  (menu, start_page, page, page_html)
            # check with lowercase pages names:
            topmenu_name = page.split('_')[0]
            menu_nice_name = menu
            if len(menu) < 3:
                 menu_nice_name = "&nbsp;" + menu + "&nbsp;"  # make a little longer for showing
             
            if topmenu_name == menu.lower() or page == start_page:
                if page_html == start_page:
                    tr.append(TD(menu_nice_name, Class="topon"))
                else:
                    tr.append(TD(Href(start_page, menu_nice_name), onclick="location='%s';"%start_page, Class="topon", height=h2))
            else:
                tr.append(TD(Href(start_page, menu_nice_name), onclick="location='%s';"%start_page, Class="topoff", height=h2))
            tr.append(TD('&nbsp;', Class="blank"))
        tl_menu.append(tr)
        tl.append(TR(TD(tl_menu, Class="topmenu")))
        return tl

    def html_footer(self, page, part, nice_name):
        """construct a footer for the webpage"""
        page_type = part
        page_html = page
        
        tl = FullTable(Class="footer")
        if page_type == 'index' and page == 'index.html':
            scramble = 'home'
        else:
            scramble = join(Href('index.html', 'home'), ' &gt; ', nice_name)
        scramble += " &gt; " + str(Href("javascript:scrollTo(0,0);", " top"))
        tim = time.localtime(time.time())
        copyright = time.strftime("%a, %d %b %Y", tim)
        tl.append(TR(TD(scramble, Class="scramble"),
                     TD(copyright, Class="copyright")))
        return join(tl)

    def get_first_page(self, menu):
        """Extract from the list of pages the first page to display"""
        if menu in self.left_menu_keys:
            return "%s_overview.html"% menu.lower()
##            key = self.left_menu_keys[menu][0]
##            return self.left_menu[menu][key]
        else:
            return 'index.html'
        

##    def sort_by_spoken(self, cmds):
##        """sort list of tuples by the first key
##
##        note the presentation is the other way
##        each cmd is (spoken, written), but presentation in
##        html is (written, spoken)
##
##        """
##        cmds.sort()
##        return cmds
##
##    def sort_by_written(self, cmds):
##        """sort list of tuples by the second key
##        
##        note the presentation is the other way
##        each cmd is (spoken, written), but presentation in
##        html is (written, spoken)
##
##        """
##        decorated = [(s,w) for (w,s) in cmds]
##        decorated.sort()
##        undecorated = [(s,w) for (w,s) in decorated]
##        return undecorated
##
##    def sort_csc_values_by_scope(self, csc_values):
##        """sort list of tuples by skope
##
##        [(str context_equivalence, str skop, str docs), ...]
##        """
##        for  csc_value in  csc_values:
##            a, skope, c = csc_value
##            if not valid_scope(skope):
##                trace("WhatCanISay.sort_csc_value_by_scope",
##                      "WARNING: invalid skope in csc entry WhatCanISay: %s"% \
##                      repr(csc_value))
##                return csc_values
##        scope_order_list = scope_order()
##        dec = [(scope_order_list.index(skope), (a, skope, c)) for (a, skope, c) in csc_values]
##        dec.sort()
##        return [b for (a,b) in dec]

# defaults for vim - otherwise ignore
# vim:sw=4
