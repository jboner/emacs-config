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

<xsl:stylesheet  
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">  

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
	<div class="header">Transcript
	</div>
	<xsl:apply-templates select="*"/>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="*">
    <xsl:apply-templates select="@* | *"/>
  </xsl:template>

  <xsl:template match="stat">
    <table border="1">
      <xsl:for-each select="@*">
	<tr  align="left">
	  <th><xsl:value-of select="local-name(.)"/></th>
	  <td><xsl:value-of select="."/></td>
	</tr>
      </xsl:for-each>
    </table>
  </xsl:template>	

  <xsl:template match="stat-detail">
    <div class="details">	
      <span class="head">Key counts:</span>
      <xsl:for-each select="key"><xsl:value-of
select="@name"/>:<xsl:value-of select="@value"/>|</xsl:for-each>
</div>
  </xsl:template>
  
  <xsl:template match="session">
    <br/>
    <div class="date">
      <xsl:value-of select="@date"/>
    </div>
    <xsl:apply-templates select="* | text()"/>
  </xsl:template>

  <xsl:template match="text()">
    <xsl:value-of select="."/>
  </xsl:template>

  <xsl:template match="command">
    <div class="command">
      <xsl:value-of select="@name"/>
    </div>
    <xsl:apply-templates select="text()"/>
  </xsl:template>

</xsl:stylesheet>

