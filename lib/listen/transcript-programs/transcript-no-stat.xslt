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
  </xsl:template>	

  <xsl:template match="stat-detail">
  </xsl:template>
  
  <xsl:template match="session">
    <xsl:apply-templates select="* | text()"/>
  </xsl:template>

  <xsl:template match="text()">
    <xsl:value-of select="."/>
  </xsl:template>

  <xsl:template match="command">
    <strong>
      <xsl:value-of select="@name"/>
    </strong>
    <xsl:apply-templates select="text()"/>
    <span  xml:space="preserve"> </span>
  </xsl:template>


  <xsl:template match="command/text()">
    <span  xml:space="preserve"> </span>
    <em>
      <xsl:value-of select="."/>
    </em>
  </xsl:template>



</xsl:stylesheet>

