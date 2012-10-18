//
// WYZZ v0.1 Copyright (c) 2007 The Mouse Whisperer
//
// Contains code Copyright (c) 2006 openWebWare.com
// This copyright notice MUST stay intact for use.
//
// An open source WYSIWYG editor for use in web based applications.
// For full source code and docs, visit http://wyzz.bpweb.net
//
// This library is free software; you can redistribute it and/or modify 
// it under the terms of the GNU Lesser General Public License as published 
// by the Free Software Foundation; either version 2.1 of the License, or 
// (at your option) any later version.
//
// This library is distributed in the hope that it will be useful, but 
// WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
// or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public 
// License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along 
// with this library; if not, write to the Free Software Foundation, Inc., 59 
// Temple Place, Suite 330, Boston, MA 02111-1307 USA 

// Editor width and Height
wyzzW = 99;
wyzzH = 450;

// Style Sheet
document.write('<link rel="stylesheet" type="text/css" href="javascript/style.css">\n'); 

// Order of available commands in toolbar
var buttonName = new Array("bold","italic","underline","justifyleft","justifycenter","justifyright","insertunorderedlist","insertorderedlist","upsize","downsize");
	
/* 
Emulates insertAdjacentHTML(), insertAdjacentText() and insertAdjacentElement() three functions 
so they work with Netscape 6/Mozilla - By Thor Larholm me@jscript.dk
*/
if(typeof HTMLElement!="undefined" && !HTMLElement.prototype.insertAdjacentElement){
  HTMLElement.prototype.insertAdjacentElement = function
  (where,parsedNode)
	{
	  switch (where){
		case 'beforeBegin':
			this.parentNode.insertBefore(parsedNode,this)
			break;
		case 'afterBegin':
			this.insertBefore(parsedNode,this.firstChild);
			break;
		case 'beforeEnd':
			this.appendChild(parsedNode);
			break;
		case 'afterEnd':
			if (this.nextSibling) 
      this.parentNode.insertBefore(parsedNode,this.nextSibling);
			else this.parentNode.appendChild(parsedNode);
			break;
		}
	}

	HTMLElement.prototype.insertAdjacentHTML = function
  (where,htmlStr)
	{
		var r = this.ownerDocument.createRange();
		r.setStartBefore(this);
		var parsedHTML = r.createContextualFragment(htmlStr);
		this.insertAdjacentElement(where,parsedHTML)
	}


	HTMLElement.prototype.insertAdjacentText = function
  (where,txtStr)
	{
		var parsedText = document.createTextNode(txtStr)
		this.insertAdjacentElement(where,parsedText)
	}
};

function make_wyzz(textareaID) {
 
  // Hide the textarea 
  document.getElementById(textareaID).style.display = 'none'; 
	
  // get textareaID
  var n = textareaID;
	
  // Toolbars width is 2 pixels wider than the editor
  toolbarWidth = parseFloat(wyzzW);
	
  // Generate WYSIWYG toolbar
  var toolbar;
  toolbar =  '<table cellpadding="0" cellspacing="0" border="0" class="toolbar" style="width:' + toolbarWidth + '%;"><tr>';
  
  // Output buttons for toolbar
    for (btn in buttonName) {
            toolbar += '<td style="width: 22px;"><img src="javascript/wyzzicons/' +buttonName[btn]+ '.gif" border=0 unselectable="on" title="' +buttonName[btn]+ '" id="' +buttonName[btn]+ '" class="button" onClick="formatText(this.id,\'' + n + '\');" onmouseover="if(className==\'button\'){className=\'buttonOver\'};" onmouseout="if(className==\'buttonOver\'){className=\'button\'};" unselectable="on" width="20" height="20"></td>';
    }

  toolbar += '<td>&nbsp;</td></tr></table>';  

  // Create iframe for editor
  var iframe = '<table cellpadding="0" cellspacing="0" border="0" style="width:' + wyzzW + '%; height:' + wyzzH + 'px;border: 1px inset #CCCCCC;"><tr><td valign="top">\n'
  + '<iframe frameborder="0" id="wysiwyg' + n + '"></iframe>\n'
  + '</td></tr></table>\n';

  // Insert toolbar after the textArea
  document.getElementById(n).insertAdjacentHTML("afterEnd", toolbar + iframe);
	
  // Give the iframe the required height and width
  document.getElementById("wysiwyg" + n).style.height = wyzzH + "px";
  document.getElementById("wysiwyg" + n).style.width = wyzzW + "%";
	
  // Pass the textarea's existing text into the editor
  var content = document.getElementById(n).value;
  var doc = document.getElementById("wysiwyg" + n).contentWindow.document;
	
  // Write the textarea's content into the iframe
  doc.open();
  doc.write(content);
  doc.close();
	
  // Make the iframe editable in both Mozilla and IE
  doc.body.contentEditable = true;
  doc.designMode = "on";
	
  // Update the textarea with content in WYSIWYG when user submits form
  var browserName = navigator.appName;
  if (browserName == "Microsoft Internet Explorer") {
    for (var idx=0; idx < document.forms.length; idx++) {
      document.forms[idx].attachEvent('onsubmit', function() { updateTextArea(n); });
    }
  }
  else {
  	for (var idx=0; idx < document.forms.length; idx++) {
    	document.forms[idx].addEventListener('submit',function OnSumbmit() { updateTextArea(n); }, true);
    }
  }
};

function formatText(id, n, selected) {
  // When user clicks button make sure it always targets correct textarea
  document.getElementById("wysiwyg" + n).contentWindow.focus();	
  if(id=="upsize") {
    var currentFontSize = document.getElementById("wysiwyg"+n).contentWindow.document.queryCommandValue("FontSize");
    if(currentFontSize == '') currentFontSize = 3; // fudge for FF
    if(currentFontSize < 7) {
      var newFontSize = parseInt(currentFontSize) + 1;
      } else {
      var newFontSize = currentFontSize;
      }
      document.getElementById("wysiwyg" + n).contentWindow.document.execCommand("FontSize", false, newFontSize);
    }
  else if(id=="downsize") {
    var currentFontSize = document.getElementById("wysiwyg"+n).contentWindow.document.queryCommandValue("FontSize");
    if(currentFontSize > 1) {
      var newFontSize = currentFontSize - 1;
      } else {
      var newFontSize = currentFontSize;
      }
      document.getElementById("wysiwyg" + n).contentWindow.document.execCommand("FontSize", false, newFontSize);
    }
    else {
    document.getElementById("wysiwyg" + n).contentWindow.document.execCommand(id, false, null);
  }
};

function insertHTML(html, n) {
  var browserName = navigator.appName;	 	 
	if (browserName == "Microsoft Internet Explorer") {	  
	  document.getElementById('wysiwyg' + n).contentWindow.document.selection.createRange().pasteHTML(html);   
	} 
	 
	else {
	  var div = document.getElementById('wysiwyg' + n).contentWindow.document.createElement("div");
		 
		div.innerHTML = html;
		var node = insertNodeAtSelection(div, n);		
	}
}

function insertNodeAtSelection(insertNode, n) {
  // get current selection
  var sel = document.getElementById('wysiwyg' + n).contentWindow.getSelection();

  // get the first range of the selection
  // (there's almost always only one range)
  var range = sel.getRangeAt(0);

  // deselect everything
  sel.removeAllRanges();

  // remove content of current selection from document
  range.deleteContents();

  // get location of current selection
  var container = range.startContainer;
  var pos = range.startOffset;

  // make a new range for the new selection
  range=document.createRange();

  if (container.nodeType==3 && insertNode.nodeType==3) {

    // if we insert text in a textnode, do optimized insertion
    container.insertData(pos, insertNode.nodeValue);

    // put cursor after inserted text
    range.setEnd(container, pos+insertNode.length);
    range.setStart(container, pos+insertNode.length);
  } 
	
	else {
    var afterNode;
    
		if (container.nodeType==3) {
      // when inserting into a textnode
      // we create 2 new textnodes
      // and put the insertNode in between

      var textNode = container;
      container = textNode.parentNode;
      var text = textNode.nodeValue;

      // text before the split
      var textBefore = text.substr(0,pos);
      // text after the split
      var textAfter = text.substr(pos);

      var beforeNode = document.createTextNode(textBefore);
      afterNode = document.createTextNode(textAfter);

      // insert the 3 new nodes before the old one
      container.insertBefore(afterNode, textNode);
      container.insertBefore(insertNode, afterNode);
      container.insertBefore(beforeNode, insertNode);

      // remove the old node
      container.removeChild(textNode);
    } 
	
	  else {
      // else simply insert the node
      afterNode = container.childNodes[pos];
      container.insertBefore(insertNode, afterNode);
    }

    range.setEnd(afterNode, 0);
    range.setStart(afterNode, 0);
  }

  sel.addRange(range);
};


function updateTextArea(n) {
  document.getElementById(n).value = document.getElementById("wysiwyg" + n).contentWindow.document.body.innerHTML;
};


function grabSelectedText(n){ 
   var browserName = navigator.appName; 
   var selectedText = ''; 
   // Grab Selected Text for IE 
   if (browserName == "Microsoft Internet Explorer") { 
      var theText = document.getElementById("wysiwyg" + n).contentWindow.document.selection; 
      if(theText.type =='Text')   { 
         var newText = theText.createRange(); 
         selectedText = newText.text; 
      } 
   } 
   // Grab Selected Text for Mozilla/Netscape 
   else { 
      var selectedText = document.getElementById("wysiwyg" + n).contentWindow.document.getSelection(); 
   } 
   return selectedText; 
} 
