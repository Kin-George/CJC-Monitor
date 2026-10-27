// Data reused/trimmed from the interactive page (ProductosFinales/02/Interactivo/index.html)
// Source: Informe 02 CJC, DANE PTF, base 2015. Verified against the report's published figures.

var TOTAL_SERIES = [
  {y:2005, vab:4.7010, ptf:-0.3200},{y:2006, vab:5.9978, ptf:4.2308},{y:2007, vab:6.1237, ptf:-2.6822},
  {y:2008, vab:3.1089, ptf:-1.6541},{y:2009, vab:1.3165, ptf:-1.0518},{y:2010, vab:4.2517, ptf:-1.7028},
  {y:2011, vab:6.4878, ptf:0.4646},{y:2012, vab:3.7863, ptf:-0.4509},{y:2013, vab:5.1852, ptf:0.4602},
  {y:2014, vab:4.3596, ptf:-1.4143},{y:2015, vab:3.0752, ptf:0.7723},{y:2016, vab:2.1953, ptf:-1.1601},
  {y:2017, vab:1.3479, ptf:0.1938},{y:2018, vab:2.4643, ptf:0.2351},{y:2019, vab:3.0217, ptf:0.2295},
  {y:2020, vab:-7.5295, ptf:-1.5029},{y:2021, vab:9.7053, ptf:1.2977},{y:2022, vab:6.0771, ptf:0.1325},
  {y:2023, vab:1.1792, ptf:-0.9234},{y:2024, vab:1.6304, ptf:1.2060},{y:2025, vab:2.6560, ptf:0.7537}
];
var AGG = {vabAvg:3.39, laborAvg:1.73, capitalAvg:1.79, ptfAvg:-0.14};

// Cuadro A1 (report appendix), 9 grandes ramas, cumulative PTF growth 2005-2024 (%)
var SECTORS = [
  {code:'comercio', name:'Comercio y hotelería', cum:23.17, contribAnual:0.13},
  {code:'serv_sociales', name:'Servicios sociales y personales', cum:14.16, contribAnual:0.11},
  {code:'agro', name:'Agropecuario', cum:21.84, contribAnual:0.06},
  {code:'transporte', name:'Transporte y comunicaciones', cum:4.26, contribAnual:0.02},
  {code:'serv_publicos', name:'Servicios públicos', cum:-5.53, contribAnual:-0.01},
  {code:'construccion', name:'Construcción', cum:-11.06, contribAnual:-0.05},
  {code:'manufactura', name:'Manufactura', cum:-5.15, contribAnual:-0.06},
  {code:'mineria', name:'Minería', cum:-32.09, contribAnual:-0.11},
  {code:'financieras', name:'Financieras', cum:-19.01, contribAnual:-0.15}
];

// Counterfactual: +1pp PTF/year sustained 2005-2025 -> cumulative production effect (verified: 23.4%)
function computeCounterfactual(extraPP){
  var n = TOTAL_SERIES.length;
  var cumActual = TOTAL_SERIES.reduce(function(s,r){ return s + r.vab; }, 0);
  var cumCounterfactual = cumActual + extraPP*n;
  var diff = Math.exp((cumCounterfactual-cumActual)/100) - 1;
  return diff*100;
}
var COUNTERFACTUAL_PP = computeCounterfactual(1); // ≈ 23.4
