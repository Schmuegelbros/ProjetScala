$(function(){
	for(var i=0;i<5;i++){
		$('#tableHoraire').append("<tr>");
	}
	
	for(var i=0;i<6;i++){
		$('#tableHoraire tr').append("<td>");
	}
	
	//$('td').css({'border':'solid 1px','padding':'10px'});
	
	$.ajax({
		url:"/getHoraire",
		type:"GET",
		success:function(resp){
			console.log(resp);
		}
	});
});