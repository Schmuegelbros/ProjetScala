var horaire;
$(function(){
	for(var i=0;i<5;i++){
		$('#tableHoraire tbody tr').append("<td>");
	}
	
	$.ajax({
		url:"/getHoraire",
		type:"GET",
		success:function(resp){
			horaire=resp;
			for(key in horaire){
				$('#selectSerie').append($('<option>', {
					value: key,
					text: key
				}));
			}
			
		}
	});
	
	$('#selectSerie').on('change',function(){
		if (this.value !==null) chargerHoraireSerie(this.value);
	});
});

function chargerHoraireSerie(serie){
	//console.log(horaire[serie])
	var horaireSerie=horaire[serie];
	for(var i=0;i<20;i++){
		var indiceTd=i;
		$('#tableHoraire td').eq(indiceTd).html(horaireSerie[i][1]+"-"+horaireSerie[i][0]+"-"+horaireSerie[i][2]);
	}
}