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
# (C)2000, David C. Fox
#
##############################################################################

"""a mix-in for wxFrame which allows you to insert menus and 
menu items before others by name"""


import debug
from Object import Object
from wxPython.wx import *

class wxFrameMenuMixIn(Object):
    """a mix-in for wxFrame which allows you to insert menus and 
    menu items before others by name

    **INSTANCE ATTRIBUTES**

    *none*

    **CLASS ATTRIBUTES**

    *none*
    """
    def __init__(self, **args): 
        self.deep_construct(wxFrameMenuMixIn, {}, args)

    def insert_menu(self, menu, title, before):
        """insert a new menu into the menu bar before a menu with a given name

        **INPUTS**

        *wxMenu menu* -- new menu to insert

        *STR title* -- title of the new menu

        *STR before* -- title of the existing menu before which to insert 
        the new menu

        **OUTPUTS**

        *BOOL* -- true if the before menu is found and the new menu is
        successfully inserted
        """
        menu_bar = self.GetMenuBar()
        index = menu_bar.FindMenu(before)
        if index == -1:
            return 0
        return menu_bar.Insert(index, menu, title)

    def get_menu_by_name(self, title):
        """finds a menu by name and returns the menu (menubar.FindMenu
        finds the index by name)

        **INPUTS**

        *STR title* -- title of the menu

        **OUTPUTS**

        *wxMenu* -- the menu, or None, if the menu is not found.
        """
        menu_bar = self.GetMenuBar()
        index = menu_bar.FindMenu(title)
        if index == -1:
            return None
        return menu_bar.GetMenu(index)

    def list_item_labels(self, menu):
        """return a list of the labels from a list of menu items

        **INPUTS**

        *wxMenu menu* -- the menu

        **OUTPUTS**

        *[STR]* -- the list of labels representing 
        the contents of that menu
        """
        items = menu.GetMenuItems()
        titles = []
        for item in items:
            titles.append(item.GetLabel())
        return titles

    def find_item_index_by_label(self, menu, label):
        """returns the index of a given label in a given menu 

        **INPUTS**

        *wxMenu menu* -- the menu

        *STR label* -- label of the item within that menu

        **OUTPUTS**

        *INT* -- the index of the item, or None if not found
        """
        labels = self.list_item_labels(menu)
        for i in range(len(labels)):
            if labels[i] == label:
                return i
        return None

    def find_item_by_label(self, menu, label):
        """returns the wxMenuItem with a given label in a given menu 

        **INPUTS**

        *wxMenu menu* -- the menu

        *STR label* -- label of the item within that menu

        **OUTPUTS**

        *wxMenuItem* -- the item, or None if not found
        """
        i = self.find_item_index_by_label(menu, label)
        if i == None:
            return None
        items = menu.GetMenuItems()
        return items[i]

    def make_menu_item(self, parent_menu, ID, text, help_string = None, 
        checkable = 0, sub_menu = None):
        """create a new menu item to insert into a given parent menu

        **INPUTS**

        *wxMenu parent_menu* -- the menu into which the item is about to
        be inserted

        *INT ID* -- the wxPython ID for the item (if -1, the item will
        be a separator)

        *STR text* -- text of the menu item

        *STR help_string* -- help string for the item

        *BOOL checkable* -- is the item checkable

        *wxMenu sub_menu* -- does the item introduce a sub_menu?

        **OUTPUTS**

        *wxMenuItem* -- the new item
        """
        return wxMenuItem(parent_menu, ID, text, help_string, 
            checkable, sub_menu)

    def insert_item_before_label(self, menu_item, menu, label):
        """insert a new menu item into a menu before the item with a given 
        label

        **INPUTS**

        *wxMenuItem menu_item* -- new menu to insert

        *wxMenu menu* -- new menu to insert

        *STR label* -- label of the existing menu item before which to insert 
        the new item

        **OUTPUTS**

        *BOOL* -- true if the existing item is found and the new item is
        successfully inserted
        """
        index = self.find_item_index_by_label(menu, label)
#        print 'index ', index
        if index == None:
            return 0
        menu.InsertItem(index, menu_item)


