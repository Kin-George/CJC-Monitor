/* Deterministic frame renderer: __renderFrame(t) computes the ENTIRE visual
   state for time t from scratch (each captured frame is a fresh page load,
   so there is no persisted animation state — just a pure function of t). */
(function(){
"use strict";

var SVGNS = 'http://www.w3.org/2000/svg';
function clamp(v,a,b){ return Math.max(a,Math.min(b,v)); }
function lerp(a,b,p){ return a+(b-a)*p; }
function easeOutCubic(t){ t=clamp(t,0,1); return 1-Math.pow(1-t,3); }
function easeInOutCubic(t){ t=clamp(t,0,1); return t<0.5 ? 4*t*t*t : 1-Math.pow(-2*t+2,3)/2; }
function smoothstep(e0,e1,x){ var t=clamp((x-e0)/(e1-e0),0,1); return t*t*(3-2*t); }
function pop(localT, from, dur){ dur=dur||400; var p=smoothstep(from,from+dur,localT); return {op:p, sc:0.72+0.28*p}; }
function fmtPct(v){ var s=(v*100).toFixed(2).replace('.',','); return (v>=0?'+':'')+s+'%'; }

function el(tag, attrs, parent){
  var e = document.createElement(tag);
  for (var k in attrs) e.setAttribute(k, attrs[k]);
  if (parent) parent.appendChild(e);
  return e;
}
function svgEl(tag, attrs, parent){
  var e = document.createElementNS(SVGNS, tag);
  for (var k in attrs) e.setAttribute(k, attrs[k]);
  if (parent) parent.appendChild(e);
  return e;
}
function clearEl(node){ node.innerHTML = ''; }

// Subtitles removed by request.

/* ---------- SCENE TIMING ---------- */
var SCENES = [
  {id:'s1', start:0,     end:5000},
  {id:'s2', start:5000,  end:15000},
  {id:'s3', start:15000, end:28000},
  {id:'s4', start:28000, end:42000},
  {id:'s5', start:42000, end:52000},
  {id:'s6', start:52000, end:63000}
];
var INTERNAL_DURATION_MS = 63000;
window.__TOTAL_DURATION_MS = 90000; // paused pacing: same choreography, stretched 63s -> 90s
var TIME_STRETCH = window.__TOTAL_DURATION_MS / INTERNAL_DURATION_MS;

/* ================= SCENE 1: PORTADA ================= */
function renderS1(localT){
  var brand = document.querySelector('#s1 .safe-top');
  brand.style.opacity = smoothstep(0,350,localT);
  var title = document.getElementById('s1-title');
  title.textContent = '¿Colombia produce más por cada hora de trabajo?';
  var p = pop(localT, 300, 900);
  title.style.opacity = p.op;
  title.style.transform = 'translateY(-50%) translateY(' + ((1-p.op)*26) + 'px) scale(' + (0.94+0.06*p.op) + ')';
}

/* ================= SCENE 2: QUE ES PRODUCTIVIDAD ================= */
function renderS2(localT){
  var host = document.getElementById('s2-formula');
  clearEl(host);
  var terms = [
    {key:'ocup', icon:'👥', label:'Ocupados', sub:'Cuánta gente trabaja', from:300, gold:false},
    {key:'op1',  op:'×', from:1300},
    {key:'horas',icon:'⏱️', label:'Horas', sub:'Cuánto trabaja cada quien', from:1600, gold:false},
    {key:'op2',  op:'×', from:2600},
    {key:'hora', icon:'⚙️', label:'PIB por hora', sub:'La productividad', from:2900, gold:true},
    {key:'op3',  op:'=', from:3900},
    {key:'pib',  icon:'📈', label:'PIB total', sub:'Lo que produce el país', from:4200, gold:false}
  ];
  terms.forEach(function(term){
    var p = pop(localT, term.from, 420);
    if (p.op <= 0.01) return;
    if (term.op){
      var opEl = el('div', {}, host);
      opEl.style.cssText = 'font-size:56px;color:#8a8880;font-weight:300;opacity:'+p.op+';margin:4px 0;';
      opEl.textContent = term.op;
      return;
    }
    var row = el('div', {}, host);
    row.style.cssText = 'display:flex; align-items:center; gap:28px; padding:20px 40px; border-radius:20px; margin:6px 0;'
      + (term.gold ? 'background:rgba(227,168,58,0.14); border:2px solid rgba(227,168,58,0.5);' : 'background:rgba(255,255,255,0.05); border:2px solid rgba(255,255,255,0.08);')
      + 'opacity:'+p.op+'; transform:scale('+p.sc+'); transform-origin:center;';
    var ic = el('div', {}, row); ic.style.cssText='font-size:56px;'; ic.textContent = term.icon;
    var txt = el('div', {}, row);
    var lab = el('div', {}, txt); lab.style.cssText = 'font-size:44px; font-weight:800; color:'+(term.gold?'#e3a83a':'#ffffff')+';'; lab.textContent = term.label;
    var sub = el('div', {}, txt); sub.style.cssText = 'font-size:26px; color:#8a8f97; margin-top:4px;'; sub.textContent = term.sub;
  });
}

/* ================= SCENE 3: RESULTADO COLOMBIA ================= */
var S3_DATA = [
  {label:'Ocupados', sub:'más gente trabajando', v:0.0209, color:'#3987e5'},
  {label:'Horas por trabajador', sub:'menos horas por persona', v:-0.0045, color:'#e66767'},
  {label:'PIB por hora', sub:'más producto por hora', v:0.0151, color:'#199e70', star:true},
  {label:'PIB total', sub:'resultado', v:0.0316, color:'#6f6d66'}
];
function renderS3(localT){
  var host = document.getElementById('s3-chart');
  clearEl(host);
  var w=1000,h=1200, marginL=30,marginR=30,marginT=40,marginB=140;
  var plotW=w-marginL-marginR, plotH=h-marginT-marginB;
  var minV=-0.014, maxV=0.038;
  function yScale(v){ return marginT + plotH - ((v-minV)/(maxV-minV))*plotH; }
  var svg = svgEl('svg', {viewBox:'0 0 '+w+' '+h, width:'100%', height:'100%'}, host);
  [0,0.01,0.02,0.03].forEach(function(g){
    var y = yScale(g);
    svgEl('line', {x1:marginL,x2:w-marginR,y1:y,y2:y, stroke:'#2c2c2a','stroke-width':2}, svg);
    var t = svgEl('text', {x:marginL-14,y:y+8, 'text-anchor':'end', 'font-size':22, fill:'#8a8880'}, svg);
    t.textContent = (g*100).toFixed(0)+'%';
  });
  var zeroY = yScale(0);
  svgEl('line', {x1:marginL,x2:w-marginR,y1:zeroY,y2:zeroY, stroke:'#3f3f3c','stroke-width':3}, svg);

  var bw = plotW/S3_DATA.length;
  var barGroup = svgEl('g', {}, svg);
  var zoomP = smoothstep(6000,6800,localT) - smoothstep(8300,9000,localT);
  zoomP = clamp(zoomP,0,1);
  var starBar = null;
  S3_DATA.forEach(function(d,i){
    var cx = marginL + bw*i + bw/2;
    var barW = bw*0.5;
    var growP = easeOutCubic(smoothstep(1500+i*500, 1500+i*500+700, localT));
    var y0=zeroY, y1=yScale(d.v);
    var curFar = y0 + (y1-y0)*growP;
    var top = Math.min(y0,curFar), hgt = Math.max(0, Math.abs(curFar-y0));
    var dim = zoomP>0.02 && !d.star;
    var rect = svgEl('rect', {x:cx-barW/2, y:top, width:barW, height:hgt, rx:8, fill:d.color,
      opacity: dim ? (0.92-0.75*zoomP) : 0.94}, barGroup);
    if (d.star) starBar = {cx:cx, cy:(top+top+hgt)/2 || top};
    var lblOp = clamp((growP-0.6)/0.4,0,1) * (dim ? 0.12 : 1);
    if (lblOp>0){
      var lbl = svgEl('text', {x:cx, y: d.v>=0 ? top-16 : top+hgt+34, 'text-anchor':'middle', 'font-size':30, 'font-weight':800, fill:'#ffffff', opacity:lblOp}, svg);
      lbl.textContent = fmtPct(d.v);
    }
    var nmOp = smoothstep(200,600,localT);
    var nm = svgEl('text', {x:cx, y:h-92, 'text-anchor':'middle', 'font-size':22, 'font-weight':700, fill:'#c3c2b7', opacity:nmOp*(dim?0.35:1)}, svg);
    nm.textContent = d.label;
    var sb = svgEl('text', {x:cx, y:h-56, 'text-anchor':'middle', 'font-size':19, fill:'#8a8880', opacity:nmOp*(dim?0.25:1)}, svg);
    sb.textContent = d.sub;
  });

  if (zoomP > 0.02 && starBar){
    var scale = 1 + 0.22*zoomP;
    var originXPct = (starBar.cx/w)*100, originYPct = (starBar.cy/h)*100;
    svg.style.transformOrigin = originXPct+'% '+originYPct+'%';
    svg.style.transform = 'scale('+scale+')';
  } else {
    svg.style.transform = 'scale(1)';
  }

}

/* ================= SCENE 4: BURBUJAS ================= */
var S4_CURSOR_STOPS = [
  {code:'R+S', from:9600, to:11200},
  {code:'B',   from:11200, to:12700},
  {code:'L',   from:12700, to:14000}
];
function renderS4(localT){
  var host = document.getElementById('s4-chart');
  clearEl(host);
  var w=1000,h=1240, marginL=90,marginR=40,marginT=30,marginB=90;
  var plotW=w-marginL-marginR, plotH=h-marginT-marginB;
  var xMax=130, yMin=-5, yMax=8.5;
  function xScale(v){ return marginL+(v/xMax)*plotW; }
  function yScale(v){ return marginT+plotH-((v-yMin)/(yMax-yMin))*plotH; }
  var xThresh = TOTAL_SUMMARY.hora.v2025, yThresh = TOTAL_SUMMARY.hora.g*100;
  var svg = svgEl('svg', {viewBox:'0 0 '+w+' '+h, width:'100%', height:'100%', preserveAspectRatio:'none'}, host);

  var regions = [
    {id:'acel', x:marginL,y:marginT,w:xScale(xThresh)-marginL,h:yScale(yThresh)-marginT, fill:'rgba(217,89,38,0.12)'},
    {id:'auge', x:xScale(xThresh),y:marginT,w:(marginL+plotW)-xScale(xThresh),h:yScale(yThresh)-marginT, fill:'rgba(25,158,112,0.12)'},
    {id:'rezag',x:marginL,y:yScale(yThresh),w:xScale(xThresh)-marginL,h:(marginT+plotH)-yScale(yThresh), fill:'rgba(111,109,102,0.12)'},
    {id:'decl', x:xScale(xThresh),y:yScale(yThresh),w:(marginL+plotW)-xScale(xThresh),h:(marginT+plotH)-yScale(yThresh), fill:'rgba(57,135,229,0.12)'}
  ];
  regions.forEach(function(r){ svgEl('rect', {x:r.x,y:r.y,width:Math.max(0,r.w),height:Math.max(0,r.h),fill:r.fill}, svg); });
  svgEl('line', {x1:xScale(xThresh),x2:xScale(xThresh),y1:marginT,y2:marginT+plotH, stroke:'#8fb4de','stroke-width':2,'stroke-dasharray':'6,5', opacity:0.6}, svg);
  svgEl('line', {x1:marginL,x2:marginL+plotW,y1:yScale(yThresh),y2:yScale(yThresh), stroke:'#8fb4de','stroke-width':2,'stroke-dasharray':'6,5', opacity:0.6}, svg);

  var rMax=64, rMin=18;
  var pibMax = Math.max.apply(null, SECTORS.map(function(s){return s.pib2025;}));
  function rScale(v){ return rMin + Math.sqrt(v/pibMax)*(rMax-rMin); }

  var axisHiOp = smoothstep(4400,4700,localT) - smoothstep(6100,6400,localT);
  var yAxisHiOp = smoothstep(6100,6400,localT) - smoothstep(7800,8100,localT);
  var bigHiOp = smoothstep(7800,8100,localT) - smoothstep(9600,9900,localT);
  var biggest = SECTORS.reduce(function(a,b){ return b.pib2025>a.pib2025?b:a; });

  var bubblePos = {};
  SECTORS.forEach(function(s,i){
    var q = quadrantOf(s);
    var cx = xScale(s.hora2025), cy = yScale(s.horaG*100), rFull = rScale(s.pib2025);
    bubblePos[s.code] = {cx:cx, cy:cy, r:rFull, w:w, h:h};
    var p = pop(localT, 200+i*130, 420);
    if (p.op<=0.01) return;
    var isBig = (s.code===biggest.code);
    var ring = (isBig && bigHiOp>0.02);
    var g = svgEl('g', {opacity:p.op}, svg);
    if (ring){
      svgEl('circle', {cx:cx,cy:cy,r:rFull+14+6*Math.sin(localT/90), fill:'none', stroke:'#e3a83a','stroke-width':4, opacity:bigHiOp*0.9}, g);
    }
    svgEl('circle', {cx:cx,cy:cy,r:rFull*p.sc, fill:QUAD_COLOR[q], 'fill-opacity':0.55, stroke:QUAD_COLOR[q], 'stroke-width':3}, g);
    var SKIP_LABEL = {'T':1,'G+H+I':1,'O+P+Q':1,'C':1};
    if (!SKIP_LABEL[s.code]){
      var lY = cy-rFull*p.sc-14; if (lY<marginT+16) lY = cy+rFull*p.sc+30;
      var lbl = svgEl('text', {x:cx,y:lY,'text-anchor':'middle','font-size':21,'font-weight':800, fill:'#ffffff'}, g);
      lbl.textContent = s.short;
    }
  });

  if (axisHiOp>0.02){
    svgEl('line', {x1:marginL,x2:marginL+plotW,y1:marginT+plotH+34,y2:marginT+plotH+34, stroke:'#e3a83a','stroke-width':6, opacity:axisHiOp, 'stroke-linecap':'round'}, svg);
  }
  if (yAxisHiOp>0.02){
    svgEl('line', {x1:26,x2:26,y1:marginT,y2:marginT+plotH, stroke:'#e3a83a','stroke-width':6, opacity:yAxisHiOp, 'stroke-linecap':'round'}, svg);
  }

  // #s4-cursor and #s4-tooltip are static siblings of #s4-chart within the #s4
  // scene (never reparented) — positions below are expressed as percentages of
  // the FULL 1080x1920 scene, converted from chart-local (w x h) coordinates
  // via the chart container's known offset (must match the inline style on
  // #s4-chart in video.html: left:20 top:260 width:1040 height:1240).
  var CHART_L=20, CHART_T=260, CHART_W=1040, CHART_H=1240, SCENE_W=1080, SCENE_H=1920;
  function toScenePct(cx,cy){
    var sx = CHART_L + (cx/w)*CHART_W, sy = CHART_T + (cy/h)*CHART_H;
    return {x:(sx/SCENE_W)*100, y:(sy/SCENE_H)*100};
  }
  var tip = document.getElementById('s4-tooltip');
  var cdiv = document.getElementById('s4-cursor');
  var activeStop=null;
  for (var i=0;i<S4_CURSOR_STOPS.length;i++){
    if (localT>=S4_CURSOR_STOPS[i].from && localT<=S4_CURSOR_STOPS[i].to){ activeStop=S4_CURSOR_STOPS[i]; break; }
  }
  if (activeStop){
    var sec = SECTORS.filter(function(s){return s.code===activeStop.code;})[0];
    var pos = bubblePos[activeStop.code];
    var travelP = easeInOutCubic(clamp((localT-activeStop.from)/300,0,1));
    var prevIdx = S4_CURSOR_STOPS.indexOf(activeStop)-1;
    var fromPos = prevIdx>=0 ? bubblePos[S4_CURSOR_STOPS[prevIdx].code] : {cx:w/2, cy:h/2};
    var curCx = lerp(fromPos.cx, pos.cx, travelP);
    var curCy = lerp(fromPos.cy, pos.cy, travelP);
    var scenePos = toScenePct(curCx, curCy);
    var showTip = travelP>0.85;
    cdiv.style.left = scenePos.x+'%'; cdiv.style.top = scenePos.y+'%'; cdiv.style.opacity = 1;
    var q2 = quadrantOf(sec);
    tip.style.display = showTip ? 'block' : 'none';
    tip.style.opacity = showTip ? clamp((travelP-0.85)/0.15,0,1) : 0;
    var tipLeftPct = clamp(scenePos.x,6,58), tipTopPct = clamp(scenePos.y-9,4,80);
    tip.style.left = tipLeftPct+'%'; tip.style.top = tipTopPct+'%';
    tip.innerHTML = '<div style="font-size:30px;font-weight:800;margin-bottom:8px;">'+sec.name+'</div>'
      + '<div style="font-size:24px;color:#c3c2b7;">PIB por hora: <b style="color:#fff">'+fmtPct(sec.horaG)+'</b> anual</div>'
      + '<div style="font-size:22px;color:#8a8f97;margin-top:6px;">'+QUAD_NAME[q2]+'</div>';
  } else {
    tip.style.display='none';
    cdiv.style.opacity = 0;
  }

}

/* ================= SCENE 5: RANKING + BUSCADOR ================= */
function renderS5(localT){
  var host = document.getElementById('s5-body');
  clearEl(host);
  var eyebrow = document.getElementById('s5-eyebrow');
  var rankData = SECTORS.slice().sort(function(a,b){ return b.horaG-a.horaG; });
  var top4 = rankData.slice(0,4), bot3 = rankData.slice(-3);
  var showRank = localT < 4700;
  eyebrow.textContent = showRank ? 'Ranking de productividad' : 'Busca entre 50 actividades';

  if (showRank){
    var rankOp = 1 - smoothstep(4000,4600,localT);
    host.style.opacity = rankOp;
    var list = top4.concat(bot3);
    list.forEach(function(d,i){
      var p = pop(localT, 300+i*160, 380);
      if (p.op<=0.01) return;
      var row = el('div', {}, host);
      var up = d.horaG>=0;
      row.style.cssText = 'display:flex; justify-content:space-between; align-items:center; padding:22px 0; '
        + 'border-bottom:2px solid rgba(255,255,255,0.08); opacity:'+p.op+'; transform:translateX('+((1-p.op)*40)+'px);';
      var nm = el('div', {}, row); nm.style.cssText='font-size:34px; font-weight:700; color:#fff;'; nm.textContent = d.short;
      var val = el('div', {}, row); val.style.cssText='font-size:36px; font-weight:800; color:'+(up?'#0ca30c':'#e66767')+';'; val.textContent = fmtPct(d.horaG);
      host.appendChild(row);
    });
  } else {
    host.style.opacity = smoothstep(4200,4600,localT);
    var terms = ['transporte','salud','comercio','agricultura'];
    var slots = [[4600,5950],[5950,7300],[7300,8650],[8650,10000]];
    var slotIdx = 0;
    for (var i=0;i<slots.length;i++){ if (localT>=slots[i][0]) slotIdx=i; }
    var term = terms[slotIdx];
    var slot = slots[slotIdx];
    var typeP = clamp((localT-slot[0])/500,0,1);
    var nChars = Math.round(term.length*easeOutCubic(typeP));
    var box = el('div', {}, host);
    box.style.cssText = 'background:#171b22; border:2px solid rgba(255,255,255,0.16); border-radius:20px; padding:26px 32px; display:flex; align-items:center; gap:16px;';
    var mag = el('div',{},box); mag.style.cssText='font-size:34px;'; mag.textContent='🔍';
    var txt = el('div',{},box); txt.style.cssText='font-size:36px; color:#fff; font-weight:600;';
    txt.textContent = term.slice(0,nChars);
    var caret = el('span',{},txt); caret.style.cssText='display:inline-block;width:4px;height:34px;background:#e3a83a;margin-left:4px;vertical-align:middle;opacity:'+(typeP<1?(Math.floor(localT/180)%2):0)+';';
    var results = el('div', {style:'margin-top:26px;'}, host);
    var rows = SEARCH_DEMO[term] || [];
    var resP = clamp((localT-slot[0]-550)/400,0,1);
    rows.forEach(function(r,ri){
      var rp = clamp(resP - ri*0.25, 0, 1);
      if (rp<=0.01) return;
      var row = el('div', {}, results);
      row.style.cssText = 'display:flex; justify-content:space-between; align-items:center; padding:20px 26px; '
        + 'background:rgba(255,255,255,0.05); border-radius:14px; margin-top:14px; opacity:'+rp+'; transform:translateY('+((1-rp)*16)+'px);';
      var l = el('div',{},row);
      var nm = el('div',{},l); nm.style.cssText='font-size:28px; color:#fff; font-weight:700;'; nm.textContent=r.name;
      var tg = el('div',{},l); tg.style.cssText='font-size:22px; color:#8a8f97; margin-top:2px;'; tg.textContent=r.tag;
      var v = el('div',{},row); v.style.cssText='font-size:30px; font-weight:800; color:#0ca30c;'; v.textContent=r.v;
    });
  }

}

/* ================= SCENE 6: EMPLEO VS PRODUCTIVIDAD + CIERRE ================= */
function renderS6(localT){
  var chartHost = document.getElementById('s6-chart');
  var flash = document.getElementById('s6-flash');
  var endc = document.getElementById('s6-end');
  var eyebrow = document.getElementById('s6-eyebrow');

  var chartOp = smoothstep(0,300,localT) - smoothstep(6300,6600,localT);
  chartOp = clamp(chartOp,0,1);
  eyebrow.style.opacity = chartOp;
  chartHost.style.opacity = chartOp;
  clearEl(chartHost);
  if (chartOp>0.01){
    var w=960,h=1080, marginL=90,marginR=30,marginT=30,marginB=90;
    var plotW=w-marginL-marginR, plotH=h-marginT-marginB;
    var xs = SECTORS.map(function(s){return s.ocupG*100;});
    var ys = SECTORS.map(function(s){return s.horaG*100;});
    var xMin=Math.min.apply(null,xs)-0.5, xMax=Math.max.apply(null,xs)+0.5;
    var yMin=Math.min.apply(null,ys)-0.5, yMax=Math.max.apply(null,ys)+0.5;
    function xScale(v){ return marginL+((v-xMin)/(xMax-xMin))*plotW; }
    function yScale(v){ return marginT+plotH-((v-yMin)/(yMax-yMin))*plotH; }
    var n=xs.length, sx=0,sy=0,sxx=0,sxy=0;
    for (var i=0;i<n;i++){ sx+=xs[i]; sy+=ys[i]; sxx+=xs[i]*xs[i]; sxy+=xs[i]*ys[i]; }
    var mx=sx/n, my=sy/n;
    var slope=(sxx-n*mx*mx)? (sxy-n*mx*my)/(sxx-n*mx*mx):0;
    var intercept=my-slope*mx;
    var svg = svgEl('svg', {viewBox:'0 0 '+w+' '+h, width:'100%', height:'100%'}, chartHost);
    for (var gx=Math.ceil(xMin); gx<=xMax; gx+=2){
      svgEl('line',{x1:xScale(gx),x2:xScale(gx),y1:marginT,y2:marginT+plotH, stroke:'#2c2c2a','stroke-width':2}, svg);
    }
    for (var gy=Math.ceil(yMin); gy<=yMax; gy+=2){
      svgEl('line',{x1:marginL,x2:marginL+plotW,y1:yScale(gy),y2:yScale(gy), stroke:'#2c2c2a','stroke-width':2}, svg);
      var ly = svgEl('text',{x:marginL-14,y:yScale(gy)+8,'text-anchor':'end','font-size':20, fill:'#8a8880'}, svg); ly.textContent=gy+'%';
    }
    var xt = svgEl('text',{x:marginL+plotW/2,y:h-14,'text-anchor':'middle','font-size':22, fill:'#8a8880'}, svg); xt.textContent='Crecimiento del empleo';
    var yt = svgEl('text',{x:22,y:marginT+plotH/2,'text-anchor':'middle','font-size':22, fill:'#8a8880', transform:'rotate(-90 22 '+(marginT+plotH/2)+')'}, svg); yt.textContent='Crecimiento PIB/hora';
    var lineP = easeOutCubic(smoothstep(300,1600,localT));
    var x1=xMin,x2=lerp(xMin,xMax,lineP);
    svgEl('line',{x1:xScale(x1),x2:xScale(x2),y1:yScale(intercept+slope*x1),y2:yScale(intercept+slope*x2), stroke:'#e66767','stroke-width':4,'stroke-dasharray':'10,7', opacity:0.85}, svg);
    SECTORS.forEach(function(s,i){
      var p = pop(localT, 350+i*70, 350);
      if (p.op<=0.01) return;
      svgEl('circle',{cx:xScale(s.ocupG*100), cy:yScale(s.horaG*100), r:14*p.sc, fill:'#3987e5','fill-opacity':0.65, stroke:'#3987e5','stroke-width':3, opacity:p.op}, svg);
    });
  }

  var flashOp = smoothstep(6600,6900,localT) - smoothstep(7100,7300,localT);
  flashOp = clamp(flashOp,0,1);
  flash.style.display = flashOp>0.01 ? 'flex' : 'none';
  flash.style.opacity = flashOp;

  var endOp = smoothstep(7600,8200,localT);
  endc.style.display = endOp>0.01 ? 'flex' : 'none';
  endc.style.opacity = endOp;
  endc.style.transform = 'scale(' + (0.94+0.06*endOp) + ')';

}

/* ================= DISPATCH ================= */
var RENDERERS = {s1:renderS1, s2:renderS2, s3:renderS3, s4:renderS4, s5:renderS5, s6:renderS6};

window.__renderFrame = function(tExternal){
  tExternal = clamp(tExternal, 0, window.__TOTAL_DURATION_MS);
  var t = tExternal / TIME_STRETCH; // map the paused 90s clock back onto the original 63s choreography
  var active = SCENES[SCENES.length-1];
  for (var i=0;i<SCENES.length;i++){
    if (t>=SCENES[i].start && t<SCENES[i].end){ active = SCENES[i]; break; }
    if (i===SCENES.length-1) active = SCENES[i];
  }
  SCENES.forEach(function(sc){
    document.getElementById(sc.id).classList.toggle('on', sc.id===active.id);
  });
  var localT = t - active.start;
  var dur = active.end - active.start;
  var envelope = smoothstep(0,260,localT) * (1 - smoothstep(dur-220,dur,localT));
  var sceneEl = document.getElementById(active.id);
  sceneEl.style.opacity = Math.max(0.001, envelope);
  RENDERERS[active.id](localT);
};

// Auto-invoke on load using the time encoded in the URL hash (#t=12345)
var m = /t=(\d+)/.exec(location.hash);
window.__renderFrame(m ? parseInt(m[1],10) : 0);

})();
