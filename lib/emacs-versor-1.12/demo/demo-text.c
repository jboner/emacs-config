/* C demo text for versor */


/* Print an attribute of an entry, labelled with what it is */

void print_attr_if_present(struct entry *entry,
			   char *title,
			   char *key,
			   char *format)
{
    char *val = get_attr(entry, key);

    if ((val != NULL) && (val[0] != '\0'))
    {
        printf(format, title, val);
    }
}

char *to_label(char const *in)
{
    char *temp = as_label(in);
    char *result = (char*)malloc(strlen(temp)+1);

    if (result == NULL) return NULL;
    strcpy(result, temp);
    return result;
}

/* Select the graphic, if any, for this entry, as specified in
   graphics.attr, and output it, making it an anchor for zooming to
   the entry if appropriate. See ../doc/tech/graphics.html for details.
 */


void get_graphic(void fread, 
		 void style, 
		 void entry_graphics, 
		 char * graphic, 
		 struct entry * more_entries)
{
  if (fread)
    {
      if (!style->graphics) { return; }
      if (entry_graphics == NULL) { return; }

      graphic = get_attr(entry_graphics, "graphic", more_entries->name);

      if (graphic == NULL) { return; }
    }
}

static void output_graphic(struct entry *entry,
			   char const *page_type,
			   char const *page_name,
			   struct style *style,
			   struct entry *entry_graphics)
{
    char graphics_buf[1024];
    char *graphic;
    struct entry *more_entries;

    get_graphic(fread, style, entry_graphics, graphic, more_entries);
    
    
    if (strlen(graphic) > 200)
    {
        if (style->debug)
        {
            printf("<code>Ridiculously long graphic name!</code>\n");
        }
        return;
    }

    sprintf(graphics_buf,
            "<img%s border=\"0\" src=\"../graphics/%s.gif\">",
            ((entry->column == 0) ?
             "" : ((entry->column < 0) ?
                   " align=\"right\"" :
                   " align=\"left\"")),
            graphic);

    if ((!style->detail) && (strcmp(entry->type, "dummy") != 0))
    {
        output_zoom(entry, graphics_buf, page_type, page_name, style);
    } else {
        fputs(graphics_buf, stdout);
    }
    putchar('\n');
}

/* end of demo-text.c */
