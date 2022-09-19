var c = document.getElementById("myCanvas");
var ctx = c.getContext("2d");
var dim = 590;
var size = 18;

Shiny.addCustomMessageHandler("size", function(n) {size = n});

function resetGame(size) {
  ctx.fillStyle = "white";
  ctx.fillRect(0, 0, dim, dim);
  
  ctx.beginPath();
  for (let i = 0; i <= size; i++) {
    ctx.moveTo(i*dim/size,0);
    ctx.lineTo(i*dim/size,dim);
    ctx.moveTo(0,i*dim/size);
    ctx.lineTo(dim,i*dim/size);
  }
  ctx.strokeStyle = "#c4c4c4";
  ctx.stroke();
}

Shiny.addCustomMessageHandler("resetGame", function(size) {resetGame(size)});

var onCells = [];

function updateCell(cell) {
  onCells.push(cell)
}

Shiny.addCustomMessageHandler("update", function(cell) {updateCell(cell)});

function updateGame() {
  resetGame(size)
  
  ctx.beginPath();
  var l = onCells.length;
  for(var i = 0; i < l; i++) {
    i1 = Math.floor(onCells[i]/100) - 1;
    j1 = onCells[i] % 100 - 1;
    ctx.rect(i1*dim/size, j1*dim/size, dim/size, dim/size);
  }
  ctx.fillStyle = "black";
  ctx.fill();
  onCells = [];
}

Shiny.addCustomMessageHandler("updateGame", function(v) {updateGame()});

function ColorToHex(color) {
  var hexadecimal = color.toString(16);
  return hexadecimal.length == 1 ? "0" + hexadecimal : hexadecimal;
}

function ConvertRGBtoHex(red, green, blue) {
  return "#" + ColorToHex(red) + ColorToHex(green) + ColorToHex(blue);
}

function heat(cell) {
  i = (Math.floor(cell/100) % 100) - 1;
  j = (cell % 100) - 1;
  c = (Math.floor(cell/10000))/1000;
  ctx.fillStyle = ConvertRGBtoHex(255,
    Math.floor(255*(1-c)), Math.floor(255*(1-c)));
  ctx.fillRect(i*dim/size, j*dim/size, dim/size, dim/size);
}

Shiny.addCustomMessageHandler("heat", function(n) {heat(n)});
