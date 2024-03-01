<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="html" 
    doctype-system="http://www.w3.org/TR/2000/REC-xhtml1-20000126/DTD/xhtml1-strict.dtd" doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"/>

  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml"
      xmlns:html="http://www.w3.org/1999/xhtml">
      <head>
        <title><xsl:value-of select="article/title[position()=1]"/></title>
        <link rel="stylesheet" type="text/css" href="../../../doc/styles/caddis.css" />
      </head>
      <body>
        <!--<div id="header">
          <iframe src="../../../doc/images/banner-caddis.svg" width="100%"/>
        </div>-->
        <!--<div id="header">
          <xsl:copy-of select="document('../../../doc/images/banner-caddis.svg',/)/*"/>
        </div>
        <xsl:copy-of select="."/>-->
      <!--<xsl:apply-templates/>-->
        <xsl:apply-templates/>
      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="body">
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="article">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="title">
    <h1><xsl:apply-templates/></h1>
  </xsl:template>
  
  <xsl:template match="svg">
    <!--<xsl:value-of select="'foo'"/>-->
    <xsl:copy-of select="."/>
  </xsl:template>

  <xsl:template match="sect1">
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="table">
    <table class="results"><xsl:apply-templates/></table>
  </xsl:template>
  
  <xsl:template match="caption">
    <caption><xsl:apply-templates/></caption>
  </xsl:template>
  
  <xsl:template match="thead">
    <thead><xsl:apply-templates/></thead>
  </xsl:template>
  
  <xsl:template match="tgroup">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="tbody">
    <tbody><xsl:apply-templates/></tbody>
  </xsl:template>

  <xsl:template match="row">
    <tr>
      <xsl:for-each select="entry">
        <xsl:choose>
		      <xsl:when test="position()=1">
		        <td class="rownames"><xsl:apply-templates/></td>    
		      </xsl:when>
		      <xsl:otherwise>
		        <td class="results"><xsl:apply-templates/></td>    
		      </xsl:otherwise>
		    </xsl:choose>
      </xsl:for-each>
    </tr>
  </xsl:template>

  <xsl:template match="span">
    <xsl:element name="span">
       <xsl:attribute name="class">
         <xsl:value-of select="@class"/>
       </xsl:attribute>
       <xsl:attribute name="style">
         <xsl:value-of select="@style"/>
       </xsl:attribute>
        <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="tip">
    <xsl:element name="a">
       <xsl:attribute name="href">
         <xsl:value-of select="'#'"/>
       </xsl:attribute>
       <xsl:attribute name="class">
         <xsl:value-of select="'tip'"/>
       </xsl:attribute>
       <xsl:attribute name="title">
         <xsl:value-of select="@title"/>
       </xsl:attribute>
       <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="entry">
    <td class="results"><xsl:apply-templates/></td>    
  </xsl:template>

  <xsl:template match="para">
    <p class="Textbody"><xsl:apply-templates/></p>
  </xsl:template>

  <xsl:template match="code">
    <xsl:choose>
      <xsl:when test="@language='text/html'">
        <xsl:copy-of select="child::node()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="pre">
          <xsl:apply-templates/>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>  
  
</xsl:stylesheet>