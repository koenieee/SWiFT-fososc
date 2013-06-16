$(document).ready(function(){
	  $('li.menuitem').click(function()
	  {
		if($(this).text().indexOf('Study Chosen Constitution') > -1)
		{
		//	alert('blaat');
			window.open('/studyConstitution?false=true','_blank'); //todo hide menu
			
			//$('div.column.span-6.colborder.sidebar').css('display', 'none');
		}
		
	  }
	 
	  );
	   
	  });
	  
function getURLParameter(name) {
  return decodeURIComponent((new RegExp('[?|&]' + name + '=' + "([^&;]+?)(&|#|;|$)").exec(location.search)||[,""])[1].replace(/\+/g, '%20'))||null
}
