<?xml version="1.0"?>

<!DOCTYPE xsl:stylesheet [ 

<!ENTITY space '<xsl:text
xmlns:xsl="http://www.w3.org/XSL/Transform/1.0"><![CDATA[ ]]></xsl:text>'>

<!ENTITY spacing '.8ex'>
<!ENTITY user 'font-family: Arial, Helvetica, Sans-serif; font-style:normal;
	  font-weight:normal; color: black;'>

<!ENTITY mycolor 'color: blue;'>
<!ENTITY mystyle '
font-family: Arial, helvetica,
	    sans-serif; font-weight: normal;'>
]
>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">  
  <xsl:output method="html"  encoding="UTF-8" indent="no"/> 
  <xsl:strip-space elements="*"/>
  <xsl:preserve-space elements="span"/>

  <xsl:template match="/">
    <html>  
      <head>
	<title>Sessions</title>
        <style type="text/css">
	  .command {font-weight: bold;}
	  .header, .date {font-size: 120%; &mycolor;}
	  .details {font-size: 75%;}
	  .head {font-weight: bold; &mycolor;}
	</style>
      </head>
      <body>
	(setq transcript-data '(<xsl:apply-templates select="*"/>))
      </body>
    </html>
  </xsl:template>

  <xsl:template match="*">
    <xsl:apply-templates select="@* | *"/>
  </xsl:template>

  <xsl:template match="session">
    <xsl:apply-templates select="child::node()[1]">
      <xsl:with-param name="seq-number" select="0"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="session/*">
    <xsl:apply-templates select="following-sibling::node()[1]">
      <xsl:with-param name="seq-number" select="0"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="session/ text()">
    <xsl:param name="seq-number"/>
    <xsl:if test="not($seq-number=0)">
      <xsl:value-of select="$seq-number"/><br/>
    </xsl:if>
    <xsl:apply-templates select="following-sibling::node()[1]">
      <xsl:with-param name="seq-number" select="0"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="session/command">
    <xsl:param name="seq-number"/>
    "<xsl:value-of select="@name"/>"<br/>
    <xsl:text> </xsl:text>	
<!--     <xsl:apply-templates select="text()"/> -->
    <xsl:text> </xsl:text>
    <xsl:apply-templates select="following-sibling::node()[1]">
      <xsl:with-param name="seq-number" select="$seq-number + 1"/>
    </xsl:apply-templates>
  </xsl:template>
  
</xsl:stylesheet>


