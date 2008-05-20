"""Use this simple program to experiment with wxPython"""

from wxPython.wx import *
class MainWindow(wxFrame):
    """ We simply derive a new class of Frame. """
    def __init__(self,parent,id,title):
        wxFrame.__init__(self,parent, wxNewId(), title, size = ( 200,100),
                                     style=wxDEFAULT_FRAME_STYLE|wxNO_FULL_REPAINT_ON_RESIZE)
       
        main_sizer = wxBoxSizer(wxVERTICAL)

        self.items = [('a1', 'a2', 'a3', 'a4'), ('b1', 'b2' 'b3', 'b4')]
        self.list = wxListCtrl(self, wxNewId(), wxDefaultPosition,
            wxDefaultSize, 
            style = wxLC_REPORT | wxLC_HRULES | wxLC_SINGLE_SEL)
        
        self.list.InsertColumn(0, "")
        self.list.InsertColumn(1, "Written form")

        for ii in range(len(self.items)):
           self.list.InsertStringItem(ii, "%d" % (ii+1))
           self.list.SetStringItem(ii, 1, self.items[ii][0])
           self.list.SetStringItem(ii, 1, self.items[ii][1])

        self.list.SetColumnWidth(0, wxLIST_AUTOSIZE_USEHEADER)
        self.list.SetColumnWidth(1, wxLIST_AUTOSIZE_USEHEADER)

        self.list.ScrollList(0, len(self.items))
        main_sizer.Add(self.list, 1, wxEXPAND | wxALL)
        
        EVT_LIST_ITEM_ACTIVATED(self.list, self.list.GetId(), 
                                self.on_item_activated)
        EVT_LIST_ITEM_SELECTED(self.list, self.list.GetId(), 
                               self.on_item_selected)
                               

        self.Show(true)
        
    def on_item_activated(self, event):
        print "-- on_item_activated: event=%s" % event
  
    def on_item_selected(self, event):
        print "-- on_item_selected: event=%s" % event

app = wxPySimpleApp()
frame = MainWindow(None, -1, "Small editor")
app.MainLoop()