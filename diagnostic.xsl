<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
  <html>
  <body>
    <h2>MALLET LDA model diagnostics</h2>
    <table id="topics">
      <tr>
        <th>id</th>
        <th>tokens</th>
        <th>document_entropy</th>
        <th>word-length</th>
        <th>coherence</th>
        <th>uniform_dist</th>
        <th>corpus_dist</th>
        <th>eff_num_words</th>
        <th>token-doc-diff</th>
        <th>rank_1_docs</th>
        <th>allocation_ratio</th>
        <th>allocation_count</th> 
      </tr>
      <xsl:for-each select="model/topic">
        <tr>
          <td><xsl:value-of select="@id"/></td>
          <td><xsl:value-of select="@tokens"/></td>
          <td><xsl:value-of select="@document_entropy"/></td>
          <td><xsl:value-of select="@word-length"/></td>
          <td><xsl:value-of select="@coherence"/></td>
          <td><xsl:value-of select="@uniform_dist"/></td>
          <td><xsl:value-of select="@corpus_dist"/></td>
          <td><xsl:value-of select="@eff_num_words"/></td>
          <td><xsl:value-of select="@token-doc-diff"/></td>
          <td><xsl:value-of select="@rank_1_docs"/></td>
          <td><xsl:value-of select="@allocation_ratio"/></td>
          <td><xsl:value-of select="@allocation_count"/></td>
        </tr>
      </xsl:for-each> 
    </table>

      <xsl:for-each select="model/topic">
        <h2>Topic <xsl:value-of select="@id"/></h2>
        <table class="topic_words">
          <tr>
             <th>word</th>
             <th>rank</th>
             <th>count</th>
             <th>prob</th>
             <th>cumulative</th>
             <th>docs</th>
             <th>word-length</th>
             <th>coherence</th>
             <th>uniform_dist</th>
             <th>corpus_dist</th>
             <th>token-doc-diff</th>
          </tr>
          <xsl:for-each select="word"> 
            <tr>
              <td><xsl:value-of select="."/></td>
              <td><xsl:value-of select="@rank"/></td>
              <td><xsl:value-of select="@count"/></td>
              <td><xsl:value-of select="@prob"/></td>
              <td><xsl:value-of select="@cumulative"/></td>
              <td><xsl:value-of select="@docs"/></td>
              <td><xsl:value-of select="@word-length"/></td>
              <td><xsl:value-of select="@coherence"/></td>
              <td><xsl:value-of select="@uniform_dist"/></td>
              <td><xsl:value-of select="@corpus_dist"/></td>
              <td><xsl:value-of select="@token-doc-diff"/></td>
            </tr>
          </xsl:for-each>
        </table>
      </xsl:for-each>

  </body>
  </html>
</xsl:template>

</xsl:stylesheet>
