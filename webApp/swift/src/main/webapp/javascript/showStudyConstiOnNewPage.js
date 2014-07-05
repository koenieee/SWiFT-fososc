$(document).ready(function(){
	  $('li.menuitem').click(function()
	  {
		if($(this).text().indexOf('Study Chosen Constitution') > -1)
		{

			var new_window = window.open('/studyConstitution?false=true',"Reading"); //todo hide menu
		//	alert(new_window.location);
			if (new_window.opener.closed)
			{
			//	new_window.location = "/studyConstitution?false=true";
				new_window.focus();
			}
			else
			{
				new_window.focus();
			}
            return false;
		}
		
	  }
	  
	 
	  );
	   
	  });
	  
function getURLParameter(name) {
  return decodeURIComponent((new RegExp('[?|&]' + name + '=' + "([^&;]+?)(&|#|;|$)").exec(location.search)||[,""])[1].replace(/\+/g, '%20'))||null
}
