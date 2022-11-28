src="https://cdn.rawgit.com/Herst/d3-zoom/ac2cab0ce1f1515312ec68b4e5594d19dd473dd2/build/d3.js";
import * as d3 from 'd3'
var nombreH =2;

var color = ['#00429d', '#2e59a8', '#4771b2', '#5d8abd', '#73a2c6', '#8abccf', '#a5d5d8', '#c5eddf', '#ffffe0'];

var margeX = 50;
var margeY = 50 ; 
var margeYElement = 80 ;
var margeGauche = 100 ;
var largeurPrior = 100 ;
var MAP_HEIGHT = 700;
var MAP_WIDTH = 1200;
var hauteurZone = MAP_HEIGHT-50;
var largeurZone = MAP_WIDTH-50;
var total = hauteurZone-40;
var MIN_TRANSLATE_X = 0;

var MAX_TRANSLATE_Y = total+margeY+5;
var MIN_TRANSLATE_Y = margeY;


var MIN_RECT_HEIGHT = 0;

var HANDLE_R = 5;
var HANDLE_R_ACTIVE = 12;




 var tailleDepart = total/(nombreH); 

 arrSum = function(arr){
return arr.reduce(function(a,b){
return a + b;
}, 0);
}


creationDonnees = function(nombreH){
var data = [];
for(i =0 ; i<nombreH;i++){
data[i] = {id : i, x:margeX+margeGauche ,y : margeY+5+(i*tailleDepart), width : largeurPrior ,  height : tailleDepart, prior : true,posterior : false};

}

for(i=nombreH ; i<=nombreH*2-1;i++){
data[i] = {id : i, x:margeYElement+margeX +100+margeGauche,y : margeY+5+((i-nombreH)*tailleDepart), width : tailleDepart,  height : tailleDepart, prior : false, posterior : false};

}

for(i=nombreH*2 ; i<=nombreH*3-1;i++){
data[i] = {id : i-nombreH,
x: tailleDepart*nombreH+margeYElement+margeX +100+margeYElement+margeGauche,
y : margeY+5+(tailleDepart*(i-2*nombreH)),
height : tailleDepart,
width : largeurPrior, prior : false, posterior : true};

}

return(data);
};
var data   = creationDonnees(nombreH);


var svg = d3.select("svg");

// for the background
svg.append("rect")
.style("fill", "grey")
.attr("width", largeurZone+margeX*2+20)
.attr("height", hauteurZone+ margeY*2+10);

var g = svg.append("g");

g.append("rect")
.style("fill", "white")
.attr("x", margeX)
.attr("y", margeY)
.attr("width", largeurZone+20)
.attr("height", hauteurZone+10);

g.append("rect")
.style("fill", "white")
.style("stroke", "black")
.attr("x", margeX+largeurPrior+margeYElement+margeGauche)
.attr("y", margeY+5)
.attr("width", total)
.attr("height", total);



function resizerHover() {
var el = d3.select(this), isEntering = d3.event.type === "mouseenter";
el
  .classed("hovering", isEntering)
  .attr(
	"r",
	isEntering || el.classed("resizing") ?
	  HANDLE_R_ACTIVE : HANDLE_R
  );
}

function rectResizeStartEnd() {
var el = d3.select(this), isStarting = d3.event.type === "start";
d3.select(this)
  .classed("resizing", isStarting)
  .attr(
	"r",
	isStarting || el.classed("hovering") ?
	  HANDLE_R_ACTIVE : HANDLE_R
  );
}

function rectResizing(d) {

var dragX = Math.max(
  Math.min(d3.event.x, tailleDepart*nombreH+margeX+largeurPrior+margeYElement+margeGauche),
  MIN_TRANSLATE_X
);
if (d3.select(this).classed("bottomright")) {
var dragY = Math.max(
  Math.min(d3.event.y, data[d.id+1].y+data[d.id+1].height),
  MIN_TRANSLATE_Y
);
}
if (d3.select(this).classed("middleright")) {


d.width = Math.max(dragX - d.x, MIN_RECT_HEIGHT);  
} else {
  d.height = Math.max(dragY - d.y, MIN_RECT_HEIGHT);     
}

update();
}




function update() {

var pX=0;


data[nombreH].height = data[0].height;
for(var i = 1; i <= nombreH; i++){
 pX=pX+data[nombreH-1+i].width*data[nombreH-1+i].height ;
 if(data[i].prior){
 
 var diffY = data[i-1].y +data[i-1].height-data[i].y;
data[i].y = data[i].y  +diffY;
data[i].height =data[i].height-diffY;
data[i+nombreH].height = data[i].height;
data[i+nombreH].y = data[i].y;
 }
 
 
//data[i].height= Math.min( data[i].height , MAX_TRANSLATE_Y -(data[i].height+data[i].y));
}
   for(var i = nombreH*2; i < nombreH*3; i++){

if(i>nombreH*2){data[i].y =  data[i-1].y +data[i-1].height}
data[i].height = data[i-nombreH].height*data[i-nombreH].width/(pX+0.000001)*nombreH*tailleDepart;
 }
 
 
//data[i]. 

data[nombreH-1].height = MAX_TRANSLATE_Y-data[nombreH-1].y ;

var rects = g
  .selectAll("g.rectangle")
  .data(data, function (d) {
	return d;
  });

rects.exit().remove();


var newRects =
  rects.enter()
	.append("g")
	.classed("rectangle", true);

newRects
  .append("rect")
  .classed("bg", true)
  .attr("fill", function (d) {
   if(d.prior){
	return color[d.id];
   }else{
	return color[d.id-nombreH];
   }
  })
  .attr("stroke", "black")
  .attr("stroke-width", 2)
  
  .call(d3.drag()
	.container(g.node())
   
  );

  
  
  

newRects
  .append("g")
  .classed("circles", true)
  .each(function () {
	var circleG = d3.select(this);


circleG
	  .append("circle")
	  .classed("bottomright", function (d) { if(d.id < nombreH){
	return (true);
  }else{return(false);}
  })
	  .classed("middleright", function (d) { if(d.id >= nombreH){
	return (true);
  }else{return(false);}
  })
	  .attr("r", HANDLE_R)
	  .on("mouseenter mouseleave", resizerHover)
	  .call(d3.drag()
		.container(g.node())
		.subject(function () {
		  return {x: d3.event.x, y: d3.event.y};
		})
		.on("start end", rectResizeStartEnd)
		.on("drag", rectResizing)
	  );

  });
  
  
	  newRects
  .append("g")
  .classed("texts", true)
  .each(function () {
	var textG = d3.select(this);


textG
	  .append("text")
	
	  
	  .call(d3.drag()
		.container(g.node())
		.subject(function () {
		  return {x: d3.event.x, y: d3.event.y};
		})

	  );

  });
  
  

var allRects = newRects.merge(rects);
allRects
  .attr("transform", function (d) {
	return "translate(" + d.x+ "," + d.y + ")";
  });

allRects
  .select("rect.bg")
  .attr("height", function (d) {
	return d.height;
  })
  .attr("width", function (d) {
	return d.width;
  });


allRects
  .select("circle.bottomright")
  .attr("r",function (d) { if(d.id == nombreH-1){
	return (0);
  }else{return(5);}
  })
  .attr("cx", function (d) {
	return (d.id +1) *d.width/nombreH;
  })
  .attr("cy", function (d) {
	return d.height;
  });
allRects
  .select("circle.middleright")
	  .attr("r",function (d) { if(d.posterior){
	return (0);
  }else{return(5);}
  })
  .attr("cx", function (d) {
	return (d.width) ;
  })
  .attr("cy", function (d) {
	return (d.height/2);
  });
	  allRects
  .select("text")
	  .text( function (d){if(d.posterior   ){ 
	return ("H"+(d.id-nombreH)+" : "+Math.round(1000*d.height/total)/10+"%") ;
   
	  }else if (d.prior) {
		return ("H"+d.id+" : " +Math.round(1000*d.height/total)/10+"%") ;
	   
  }else{
 
	  return ("H"+(d.id-nombreH)+" : "+Math.round(1000*d.width/total)/10+"%") ;


  }
  })
  .attr("x", function (d){if(  d.prior ){ 
	return (-80) ;
  }else if(d.posterior){
   return (+105) ;
  }else{
   if(d.width>0.75*total){
	  return (d.width-78) ;
	  }else{
	   return(d.width+10)
	  }
  }
  })
  
  .attr("y", function (d) {
	return (d.height/2);
  });


}

function controlChange() {
data[0][this.id] = +this.value;
update();
}



g.append("text")

.attr("x", 150)
.attr("y", margeY+635)
  .text("Hypothèses")
			 .attr("font-family", "sans-serif")
			 .attr("font-size", "20px")
			.attr("fill", "black");
			
			 g.append("text")

.attr("x", 400)
.attr("y", margeY+635)
  .text("Vraisemblance des données")
			 .attr("font-family", "sans-serif")
			 .attr("font-size", "20px")
			.attr("fill", "black");
			
			
			
			 g.append("text")

.attr("x", 1020)
.attr("y", margeY+635)
  .text("p(H|données)")
			 .attr("font-family", "sans-serif")
			 .attr("font-size", "20px")
			.attr("fill", "black");
			
	 function myFunction() {
nombreH = parseInt(document.getElementById("myNumber").value);
tailleDepart = total/(nombreH)

data   = creationDonnees(nombreH);
update();
}       
update();
