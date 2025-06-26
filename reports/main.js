// --- (The first part of your main.js file remains the same) ---

// Define the dimensions and margins for the chart
const margin = {top: 20, right: 30, bottom: 40, left: 90},
      width = 800 - margin.left - margin.right,
      height = 600 - margin.top - margin.bottom;

// Append the svg object to the body of the page
const svg = d3.select("#chart-container")
  .append("svg")
    .attr("xmlns", "http://www.w3.org/2000/svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", `translate(${margin.left},${margin.top})`);

// Path to your data file
const dataFile = 'data.csv';

// Load the external data
d3.csv(dataFile).then(function(data) {

  // =================================================================
  // Your D3.js PLOTTING CODE (remains unchanged)
  // =================================================================
  console.log(data);
  data.forEach(d => { d.value = +d.value; });
  const x = d3.scaleLinear().domain([0, d3.max(data, d => d.value)]).range([ 0, width]);
  svg.append("g").attr("transform", `translate(0, ${height})`).call(d3.axisBottom(x));
  const y = d3.scaleBand().range([ 0, height ]).domain(data.map(d => d.name)).padding(.1);
  svg.append("g").call(d3.axisLeft(y));
  svg.selectAll("myRect").data(data).join("rect").attr("x", x(0) ).attr("y", d => y(d.name)).attr("width", d => x(d.value)).attr("height", y.bandwidth()).attr("fill", "#69b3a2");
  // =================================================================
  
  
  // =================================================================
  // HELPER FUNCTION TO GET AND VALIDATE FILENAME
  // =================================================================
  function getFilename(extension) {
    let filename = d3.select('#filename-input').property('value');
    if (!filename || filename.trim() === "") {
        filename = "my-d3-plot"; // Default filename if input is empty
    }
    // Ensure the filename ends with the correct extension
    if (!filename.toLowerCase().endsWith(extension)) {
        filename += extension;
    }
    return filename;
  }
  
  // =================================================================
  // UPDATED CODE TO HANDLE SAVING THE PLOT
  // =================================================================

  // --- Function to Save as SVG ---
  d3.select('#save-svg-button').on('click', function() {
    const filename = getFilename(".svg");
    const svgEl = d3.select("#chart-container svg").node();
    const svgString = new XMLSerializer().serializeToString(svgEl);
    const blob = new Blob([svgString], {type: "image/svg+xml"});
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = filename; // Use the custom filename
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(url);
  });

  // --- Function to Save as PNG ---
  d3.select('#save-png-button').on('click', function() {
    const filename = getFilename(".png");
    const svgEl = d3.select("#chart-container svg").node();
    saveSvgAsPng(svgEl, filename, {
        backgroundColor: '#ffffff',
        scale: 2
    });
  });
  // =================================================================

}).catch(function(error){
   console.error("Error loading the data: " + error);
});



// --- (The first part of your main.js file remains the same) ---
// ... (plotting code) ...
// =================================================================
  
  
// =================================================================
// UPDATED CODE TO HANDLE SAVING THE PLOT (PROMPT METHOD)
// =================================================================

  // --- Function to Save as SVG ---
  d3.select('#save-svg-button').on('click', function() {
    let userInput = prompt("Enter filename for the SVG:", "my-d3-plot.svg");
    
    // If user clicks "Cancel" or enters an empty name, stop the function
    if (userInput === null || userInput.trim() === "") {
        return; 
    }

    // Ensure filename ends with .svg
    let filename = userInput.toLowerCase().endsWith('.svg') ? userInput : userInput + '.svg';
    
    const svgEl = d3.select("#chart-container svg").node();
    const svgString = new XMLSerializer().serializeToString(svgEl);
    const blob = new Blob([svgString], {type: "image/svg+xml"});
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = filename; // Use the custom filename
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(url);
  });

  // --- Function to Save as PNG ---
  d3.select('#save-png-button').on('click', function() {
    let userInput = prompt("Enter filename for the PNG:", "my-d3-plot.png");

    // If user clicks "Cancel" or enters an empty name, stop the function
    if (userInput === null || userInput.trim() === "") {
        return;
    }

    // Ensure filename ends with .png
    let filename = userInput.toLowerCase().endsWith('.png') ? userInput : userInput + '.png';

    const svgEl = d3.select("#chart-container svg").node();
    saveSvgAsPng(svgEl, filename, {
        backgroundColor: '#ffffff',
        scale: 2
    });
  });
// =================================================================

// ... (rest of the file, .catch() block) ...