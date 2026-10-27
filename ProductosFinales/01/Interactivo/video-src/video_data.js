// Data reused/trimmed from the interactive page (ProductosFinales/01/Interactivo/index.html)
// Source: Informe 01 CJC, cuadros 1-24, verified against Outputs/tables/*.csv
var TOTAL_SUMMARY = {
  hora: { v2025: 19.563, g: 0.0151 }
};

var SECTORS = [
  {code:'A', name:'Agropecuario', short:'Agropecuario', pib2025:63.895, ocupG:0.0038, hora2025:9.361, horaG:0.0327},
  {code:'B', name:'Minas', short:'Minas', pib2025:34.718, ocupG:0.0304, hora2025:48.441, horaG:-0.0309},
  {code:'C', name:'Manufactura', short:'Manufactura', pib2025:113.083, ocupG:0.0091, hora2025:19.530, horaG:0.0112},
  {code:'D+E', name:'Servicios públicos', short:'Serv. públicos', pib2025:30.300, ocupG:0.0616, hora2025:43.452, horaG:-0.0313},
  {code:'F', name:'Construcción', short:'Construcción', pib2025:41.320, ocupG:0.0339, hora2025:11.011, horaG:-0.0263},
  {code:'G+H+I', name:'Comercio, transporte y alojamiento', short:'Comercio/transporte', pib2025:179.126, ocupG:0.0246, hora2025:10.118, horaG:0.0164},
  {code:'J', name:'Información y comunicaciones', short:'Información y com.', pib2025:31.287, ocupG:-0.0007, hora2025:34.042, horaG:0.0449},
  {code:'K', name:'Financieras', short:'Financieras', pib2025:53.112, ocupG:0.0449, hora2025:54.186, horaG:0.0156},
  {code:'L', name:'Inmobiliarias', short:'Inmobiliarias', pib2025:90.074, ocupG:0.0264, hora2025:119.543, horaG:0.0085},
  {code:'M+N', name:'Profesionales y administrativas', short:'Profesionales/admin.', pib2025:70.058, ocupG:0.0465, hora2025:20.619, horaG:-0.0122},
  {code:'O+P+Q', name:'Adm. pública, educación y salud', short:'Adm. pública/salud', pib2025:165.339, ocupG:0.0254, hora2025:26.080, horaG:0.0195},
  {code:'R+S', name:'Arte, recreación y otros servicios', short:'Arte/recreación', pib2025:40.571, ocupG:0.0165, hora2025:15.961, horaG:0.0728},
  {code:'T', name:'Hogares como empleadores', short:'Hogares empleadores', pib2025:6.724, ocupG:0.0042, hora2025:4.459, horaG:0.0349}
];

function quadrantOf(s){
  var levelHigh = s.hora2025 >= TOTAL_SUMMARY.hora.v2025;
  var growHigh = s.horaG >= TOTAL_SUMMARY.hora.g;
  if (!levelHigh && growHigh) return 'acel';
  if (levelHigh && growHigh) return 'auge';
  if (!levelHigh && !growHigh) return 'rezag';
  return 'decl';
}
var QUAD_COLOR = {acel:'#d95926', auge:'#199e70', rezag:'#6f6d66', decl:'#3987e5'};
var QUAD_NAME = {acel:'Aceleradora', auge:'Líder en auge', rezag:'Rezagada', decl:'Líder en declive'};

// Search demo: curated rows per example term from the guion (42-52s scene)
var SEARCH_DEMO = {
  'transporte': [
    {name:'Transporte aéreo', tag:'Comercio/transporte', v:'+3,74%'},
    {name:'Almacenamiento y apoyo al transporte', tag:'Comercio/transporte', v:'+2,59%'}
  ],
  'salud': [
    {name:'Salud y servicios sociales', tag:'Adm. pública/salud', v:'+2,47%'}
  ],
  'comercio': [
    {name:'Comercio; reparación de vehículos', tag:'Comercio/transporte', v:'+2,76%'}
  ],
  'agricultura': [
    {name:'Ganadería', tag:'Agropecuario', v:'+5,10%'},
    {name:'Cultivos, apoyo agropecuario y mixtos', tag:'Agropecuario', v:'+2,61%'}
  ]
};
