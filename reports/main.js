// Define the dimensions and margins for the chart
const margin = {top: 20, right: 30, bottom: 40, left: 90},
      width = 800 - margin.left - margin.right,
      height = 600 - margin.top - margin.bottom;

// Append the svg object to the body of the page
const svg = d3.select("#chart-container")
  .append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", `translate(${margin.left},${margin.top})`);

// Path to your data file
const dataFile = 'data.csv'; // or 'data.json', etc.

// Load the external data
d3.csv(dataFile).then(function(data) {

  // =================================================================
  // Your D3.js PLOTTING CODE goes here
  // =================================================================
  
  // For example, you can log the data to the console to check it
  console.log(data);

  // Example of processing data (if needed)
  data.forEach(d => {
    d.value = +d.value; // Convert string to number
  });

  // Example: Create X axis
  const x = d3.scaleLinear()
    .domain([0, d3.max(data, d => d.value)])
    .range([ 0, width]);
  svg.append("g")
    .attr("transform", `translate(0, ${height})`)
    .call(d3.axisBottom(x));

  // Example: Create Y axis
  const y = d3.scaleBand()
    .range([ 0, height ])
    .domain(data.map(d => d.name))
    .padding(.1);
  svg.append("g")
    .call(d3.axisLeft(y));

  // Example: Create bars
  svg.selectAll("myRect")
    .data(data)
    .join("rect")
    .attr("x", x(0) )
    .attr("y", d => y(d.name))
    .attr("width", d => x(d.value))
    .attr("height", y.bandwidth())
    .attr("fill", "#69b3a2")


  // =================================================================
  // End of your plotting code
  // =================================================================

}).catch(function(error){
   // Handle any errors that might occur during data loading
   console.error("Error loading the data: " + error);
});