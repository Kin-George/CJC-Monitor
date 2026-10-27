/* Deterministic frame renderer for the 30-40s teaser: __renderFrame(t) computes
   the ENTIRE visual state for time t from scratch (each captured frame is a
   fresh page load). No captions/subtitles — this video is silent, meant to be
   voiced over separately with an "come explore the tool" invitation. */
(function(){
"use strict";

var SVGNS = 'http://www.w3.org/2000/svg';
function clamp(v,a,b){ return Math.max(a,Math.min(b,v)); }
function lerp(a,b,p){ return a+(b-a)*p; }
function easeOutCubic(t){ t=clamp(t,0,1); return 1-Math.pow(1-t,3); }
function easeInOutCubic(t){ t=clamp(t,0,1); return t<0.5 ? 4*t*t*t : 1-Math.pow(-2*t+2,3)/2; }
function smoothstep(e0,e1,x){ var t=clamp((x-e0)/(e1-e0),0,1); return t*t*(3-2*t); }
function pop(localT, from, dur){ dur=dur||300; var p=smoothstep(from,from+dur,localT); return {op:p, sc:0.75+0.25*p}; }
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
function fadeTag(id, localT, from){
  var e = document.getElementById(id);
  var p = smoothstep(from, from+300, localT);
  e.style.opacity = p;
  e.style.transform = 'translateY(' + ((1-p)*14) + 'px)';
}

/* ============ SCENE TIMING (35s total) ============ */
var SCENES = [
  {id:'t1', start:0,     end:4000},
  {id:'t2', start:4000,  end:10500},
  {id:'t3', start:10500, end:16000},
  {id:'t4', start:16000, end:21500},
  {id:'t5', start:21500, end:27500},
  {id:'t6', start:27500, end:35000}
];
window.__TOTAL_DURATION_MS = 35000;

/* ============ T1: HOOK ============ */
function renderT1(localT){
  var brand = document.querySelector('#t1 .safe-top');
  brand.style.opacity = smoothstep(0,300,localT);
  var title = document.getElementById('t1-title');
  title.textContent = '¿Qué tan productiva es Colombia?';
  var p = pop(localT, 250, 750);
  title.style.opacity = p.op;
  title.style.transform = 'translateY(-50%) translateY(' + ((1-p.op)*24) + 'px)';
  var sub = document.getElementById('t1-sub');
  sub.textContent = 'Una herramienta interactiva para descubrirlo';
  var p2 = pop(localT, 1100, 600);
  sub.style.opacity = p2.op;
  sub.style.transform = 'translateY(' + ((1-p2.op)*14) + 'px)';
}

/* ============ T2: BUBBLES GLIMPSE ============ */
var HERO_SECTOR_CODE = 'R+S'; // Arte, recreación y otros servicios
function renderT2(localT){
  var host = document.getElementById('t2-chart');
  clearEl(host);
  var w=1000,h=1150, marginL=90,marginR=40,marginT=20,marginB=70;
  var plotW=w-marginL-marginR, plotH=h-marginT-marginB;
  var xMax=130, yMin=-5, yMax=8.5;
  function xScale(v){ return marginL+(v/xMax)*plotW; }
  function yScale(v){ return marginT+plotH-((v-yMin)/(yMax-yMin))*plotH; }
  var xThresh = TOTAL_SUMMARY.hora.v2025, yThresh = TOTAL_SUMMARY.hora.g*100;
  var svg = svgEl('svg', {viewBox:'0 0 '+w+' '+h, width:'100%', height:'100%'}, host);

  var regions = [
    {x:marginL,y:marginT,w:xScale(xThresh)-marginL,h:yScale(yThresh)-marginT, fill:'rgba(217,89,38,0.12)'},
    {x:xScale(xThresh),y:marginT,w:(marginL+plotW)-xScale(xThresh),h:yScale(yThresh)-marginT, fill:'rgba(25,158,112,0.12)'},
    {x:marginL,y:yScale(yThresh),w:xScale(xThresh)-marginL,h:(marginT+plotH)-yScale(yThresh), fill:'rgba(111,109,102,0.12)'},
    {x:xScale(xThresh),y:yScale(yThresh),w:(marginL+plotW)-xScale(xThresh),h:(marginT+plotH)-yScale(yThresh), fill:'rgba(57,135,229,0.12)'}
  ];
  regions.forEach(function(r){ svgEl('rect', {x:r.x,y:r.y,width:Math.max(0,r.w),height:Math.max(0,r.h),fill:r.fill}, svg); });
  svgEl('line', {x1:xScale(xThresh),x2:xScale(xThresh),y1:marginT,y2:marginT+plotH, stroke:'#8fb4de','stroke-width':2,'stroke-dasharray':'6,5', opacity:0.5}, svg);
  svgEl('line', {x1:marginL,x2:marginL+plotW,y1:yScale(yThresh),y2:yScale(yThresh), stroke:'#8fb4de','stroke-width':2,'stroke-dasharray':'6,5', opacity:0.5}, svg);

  var rMax=64, rMin=18;
  var pibMax = Math.max.apply(null, SECTORS.map(function(s){return s.pib2025;}));
  function rScale(v){ return rMin + Math.sqrt(v/pibMax)*(rMax-rMin); }
  var SKIP_LABEL = {'T':1,'G+H+I':1,'O+P+Q':1,'C':1};

  var heroPos = null;
  SECTORS.forEach(function(s,i){
    var q = quadrantOf(s);
    var cx = xScale(s.hora2025), cy = yScale(s.horaG*100), rFull = rScale(s.pib2025);
    if (s.code === HERO_SECTOR_CODE) heroPos = {cx:cx, cy:cy, r:rFull, w:w, h:h};
    var p = pop(localT, 100+i*90, 350);
    if (p.op<=0.01) return;
    var g = svgEl('g', {opacity:p.op}, svg);
    svgEl('circle', {cx:cx,cy:cy,r:rFull*p.sc, fill:QUAD_COLOR[q], 'fill-opacity':0.55, stroke:QUAD_COLOR[q], 'stroke-width':3}, g);
    if (!SKIP_LABEL[s.code]){
      var lY = cy-rFull*p.sc-14; if (lY<marginT+16) lY = cy+rFull*p.sc+30;
      var lbl = svgEl('text', {x:cx,y:lY,'text-anchor':'middle','font-size':21,'font-weight':800, fill:'#ffffff'}, g);
      lbl.textContent = s.short;
    }
  });
  host.appendChild(svg);

  // single cursor + tooltip beat on the hero bubble
  var CHART_L=20, CHART_T=120, CHART_W=1040, CHART_H=1280, SCENE_W=1080, SCENE_H=1920;
  function toScenePct(cx,cy){
    var sx = CHART_L + (cx/w)*CHART_W, sy = CHART_T + (cy/h)*CHART_H;
    return {x:(sx/SCENE_W)*100, y:(sy/SCENE_H)*100};
  }
  var cdiv = document.getElementById('t2-cursor');
  var tip = document.getElementById('t2-tooltip');
  var stopFrom = 3200, stopTo = 6500;
  if (localT >= stopFrom && localT <= stopTo && heroPos){
    var travelP = easeInOutCubic(clamp((localT-stopFrom)/350,0,1));
    var startPos = {cx:w*0.5, cy:h*0.5};
    var curCx = lerp(startPos.cx, heroPos.cx, travelP), curCy = lerp(startPos.cy, heroPos.cy, travelP);
    var sp = toScenePct(curCx, curCy);
    cdiv.style.left = sp.x+'%'; cdiv.style.top = sp.y+'%'; cdiv.style.opacity = 1;
    var showTip = travelP > 0.8;
    var sec = SECTORS.filter(function(s){return s.code===HERO_SECTOR_CODE;})[0];
    tip.style.display = showTip ? 'block' : 'none';
    tip.style.opacity = showTip ? clamp((travelP-0.8)/0.2,0,1) : 0;
    tip.style.left = clamp(sp.x,6,55)+'%'; tip.style.top = clamp(sp.y-9,4,78)+'%';
    tip.innerHTML = '<div style="font-size:28px;font-weight:800;margin-bottom:6px;">'+sec.name+'</div>'
      + '<div style="font-size:23px;color:#c3c2b7;">PIB por hora: <b style="color:#fff">'+fmtPct(sec.horaG)+'</b> anual</div>';
  } else {
    cdiv.style.opacity = 0;
    tip.style.display = 'none';
  }

  fadeTag('t2-tag', localT, 250);
}

/* ============ T3: SEARCH GLIMPSE ============ */
function renderT3(localT){
  var host = document.getElementById('t3-body');
  clearEl(host);
  var term = 'salud';
  var typeP = clamp((localT-200)/500,0,1);
  var nChars = Math.round(term.length*easeOutCubic(typeP));
  var box = el('div', {}, host);
  box.style.cssText = 'background:#171b22; border:2px solid rgba(255,255,255,0.16); border-radius:24px; padding:30px 36px; display:flex; align-items:center; gap:18px;';
  var mag = el('div',{},box); mag.style.cssText='font-size:38px;'; mag.textContent='🔍';
  var txt = el('div',{},box); txt.style.cssText='font-size:42px; color:#fff; font-weight:600;';
  txt.textContent = term.slice(0,nChars);
  var caret = el('span',{},txt); caret.style.cssText='display:inline-block;width:4px;height:38px;background:#e3a83a;margin-left:4px;vertical-align:middle;opacity:'+(typeP<1?(Math.floor(localT/180)%2):0)+';';
  var results = el('div', {style:'margin-top:30px;'}, host);
  var rows = (SEARCH_DEMO[term] || []).concat([]);
  var resP = clamp((localT-800)/450,0,1);
  rows.forEach(function(r,ri){
    var rp = clamp(resP - ri*0.3, 0, 1);
    if (rp<=0.01) return;
    var row = el('div', {}, results);
    row.style.cssText = 'display:flex; justify-content:space-between; align-items:center; padding:26px 30px; '
      + 'background:rgba(255,255,255,0.05); border-radius:18px; margin-top:18px; opacity:'+rp+'; transform:translateY('+((1-rp)*18)+'px);';
    var l = el('div',{},row);
    var nm = el('div',{},l); nm.style.cssText='font-size:32px; color:#fff; font-weight:700;'; nm.textContent=r.name;
    var tg = el('div',{},l); tg.style.cssText='font-size:24px; color:#8a8f97; margin-top:4px;'; tg.textContent=r.tag;
    var v = el('div',{},row); v.style.cssText='font-size:34px; font-weight:800; color:#0ca30c;'; v.textContent=r.v;
  });
  fadeTag('t3-tag', localT, 250);
}

/* ============ T4: RANKING GLIMPSE ============ */
function renderT4(localT){
  var host = document.getElementById('t4-body');
  clearEl(host);
  var rankData = SECTORS.slice().sort(function(a,b){ return b.horaG-a.horaG; }).slice(0,5);
  rankData.forEach(function(d,i){
    var p = pop(localT, 200+i*160, 350);
    if (p.op<=0.01) return;
    var row = el('div', {}, host);
    row.style.cssText = 'display:flex; justify-content:space-between; align-items:center; padding:26px 0; '
      + 'border-bottom:2px solid rgba(255,255,255,0.08); opacity:'+p.op+'; transform:translateX('+((1-p.op)*40)+'px);';
    var nm = el('div', {}, row); nm.style.cssText='font-size:38px; font-weight:700; color:#fff;'; nm.textContent = d.short;
    var val = el('div', {}, row); val.style.cssText='font-size:40px; font-weight:800; color:#0ca30c;'; val.textContent = fmtPct(d.horaG);
    host.appendChild(row);
  });
  fadeTag('t4-tag', localT, 250);
}

/* ============ T5: SCATTER GLIMPSE ============ */
function renderT5(localT){
  var host = document.getElementById('t5-chart');
  clearEl(host);
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
  var svg = svgEl('svg', {viewBox:'0 0 '+w+' '+h, width:'100%', height:'100%'}, host);
  for (var gx=Math.ceil(xMin); gx<=xMax; gx+=2){
    svgEl('line',{x1:xScale(gx),x2:xScale(gx),y1:marginT,y2:marginT+plotH, stroke:'#2c2c2a','stroke-width':2}, svg);
  }
  for (var gy=Math.ceil(yMin); gy<=yMax; gy+=2){
    svgEl('line',{x1:marginL,x2:marginL+plotW,y1:yScale(gy),y2:yScale(gy), stroke:'#2c2c2a','stroke-width':2}, svg);
    var ly = svgEl('text',{x:marginL-14,y:yScale(gy)+8,'text-anchor':'end','font-size':20, fill:'#8a8880'}, svg); ly.textContent=gy+'%';
  }
  var xt = svgEl('text',{x:marginL+plotW/2,y:h-14,'text-anchor':'middle','font-size':22, fill:'#8a8880'}, svg); xt.textContent='Crecimiento del empleo';
  var yt = svgEl('text',{x:22,y:marginT+plotH/2,'text-anchor':'middle','font-size':22, fill:'#8a8880', transform:'rotate(-90 22 '+(marginT+plotH/2)+')'}, svg); yt.textContent='Crecimiento PIB/hora';
  var lineP = easeOutCubic(smoothstep(200,1300,localT));
  var x1=xMin,x2=lerp(xMin,xMax,lineP);
  svgEl('line',{x1:xScale(x1),x2:xScale(x2),y1:yScale(intercept+slope*x1),y2:yScale(intercept+slope*x2), stroke:'#e66767','stroke-width':4,'stroke-dasharray':'10,7', opacity:0.85}, svg);
  SECTORS.forEach(function(s,i){
    var p = pop(localT, 250+i*60, 300);
    if (p.op<=0.01) return;
    svgEl('circle',{cx:xScale(s.ocupG*100), cy:yScale(s.horaG*100), r:14*p.sc, fill:'#3987e5','fill-opacity':0.65, stroke:'#3987e5','stroke-width':3, opacity:p.op}, svg);
  });
  fadeTag('t5-tag', localT, 250);
}

/* ============ T6: CTA ============ */
function renderT6(localT){
  var brand = document.getElementById('t6-brand');
  brand.style.opacity = smoothstep(0,300,localT);
  var title = document.getElementById('t6-title');
  title.textContent = 'Explora la página interactiva';
  var p1 = pop(localT, 250, 650);
  title.style.opacity = p1.op;
  title.style.transform = 'translateY(' + ((1-p1.op)*20) + 'px)';
  var sub = document.getElementById('t6-sub');
  sub.textContent = 'Todos los datos del Informe 01, a un clic de distancia.';
  var p2 = pop(localT, 1000, 600);
  sub.style.opacity = p2.op;
  var cta = document.getElementById('t6-cta');
  cta.textContent = 'Link en la descripción';
  var p3 = pop(localT, 1700, 500);
  cta.style.opacity = p3.op;
  cta.style.transform = 'scale(' + (0.9+0.1*p3.op) + ')';
}

/* ============ DISPATCH ============ */
var RENDERERS = {t1:renderT1, t2:renderT2, t3:renderT3, t4:renderT4, t5:renderT5, t6:renderT6};

window.__renderFrame = function(t){
  t = clamp(t, 0, window.__TOTAL_DURATION_MS);
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
  var envelope = smoothstep(0,200,localT) * (1 - smoothstep(dur-180,dur,localT));
  var sceneEl = document.getElementById(active.id);
  sceneEl.style.opacity = Math.max(0.001, envelope);
  RENDERERS[active.id](localT);
};

var m = /t=(\d+)/.exec(location.hash);
window.__renderFrame(m ? parseInt(m[1],10) : 0);

})();
