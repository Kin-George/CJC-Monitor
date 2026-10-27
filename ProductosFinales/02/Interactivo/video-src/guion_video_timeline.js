/* Deterministic frame renderer: __renderFrame(t) computes the ENTIRE visual
   state for time t from scratch (each captured frame is a fresh page load,
   so there is no persisted animation state — just a pure function of t). */
(function(){
"use strict";

var SVGNS = 'http://www.w3.org/2000/svg';
function clamp(v,a,b){ return Math.max(a,Math.min(b,v)); }
function lerp(a,b,p){ return a+(b-a)*p; }
function easeOutCubic(t){ t=clamp(t,0,1); return 1-Math.pow(1-t,3); }
function smoothstep(e0,e1,x){ var t=clamp((x-e0)/(e1-e0),0,1); return t*t*(3-2*t); }
function pop(localT, from, dur){ dur=dur||400; var p=smoothstep(from,from+dur,localT); return {op:p, sc:0.72+0.28*p}; }
function fmtPct(v,d){ d=d===undefined?2:d; var s=Math.abs(v).toFixed(d).replace('.',','); return (v>=0?'+':'−')+s+'%'; }
function fmtPP(v,d){ d=d===undefined?2:d; var s=Math.abs(v).toFixed(d).replace('.',','); return (v>=0?'+':'−')+s+' pp'; }

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

/* ---------- SCENE TIMING (mirrors the new guion's second-by-second breakdown) ---------- */
var SCENES = [
  {id:'s1', start:0,     end:7000},
  {id:'s2', start:7000,  end:15000},
  {id:'s3', start:15000, end:28000},
  {id:'s4', start:28000, end:38000},
  {id:'s5', start:38000, end:54000},
  {id:'s6', start:54000, end:60000}
];
window.__TOTAL_DURATION_MS = 60000;

/* ================= SCENE 1: COLOMBIA CRECIO, PERO NO FUE MAS PRODUCTIVA ================= */
function renderS1(localT){
  var brand = document.querySelector('#s1 .safe-top');
  brand.style.opacity = smoothstep(0,350,localT);
  var title = document.getElementById('s1-title');
  title.textContent = 'Colombia creció, pero no fue más productiva.';
  var p = pop(localT, 300, 900);
  title.style.opacity = p.op;
  title.style.transform = 'translateY(-50%) translateY(' + ((1-p.op)*26) + 'px) scale(' + (0.94+0.06*p.op) + ')';
}

/* ================= SCENE 2: QUE ES LA PTF ================= */
function renderS2(localT){
  var host = document.getElementById('s2-formula');
  clearEl(host);
  var terms = [
    {key:'trabajo', icon:'👥', label:'Trabajo', sub:'Personas y horas trabajadas', from:300, gold:false},
    {key:'op1', op:'+', from:1300},
    {key:'capital', icon:'🏗️', label:'Capital', sub:'Maquinaria e infraestructura', from:1600, gold:false}
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
      + 'background:rgba(255,255,255,0.05); border:2px solid rgba(255,255,255,0.08);'
      + 'opacity:'+p.op+'; transform:scale('+p.sc+'); transform-origin:center;';
    var ic = el('div', {}, row); ic.style.cssText='font-size:56px;'; ic.textContent = term.icon;
    var txt = el('div', {}, row);
    var lab = el('div', {}, txt); lab.style.cssText = 'font-size:44px; font-weight:800; color:#ffffff;'; lab.textContent = term.label;
    var sub = el('div', {}, txt); sub.style.cssText = 'font-size:26px; color:#8a8f97; margin-top:4px;'; sub.textContent = term.sub;
  });

  // Highlight card: "La PTF mide la parte del crecimiento que no explican esos dos factores"
  var hp = pop(localT, 2600, 500);
  if (hp.op > 0.01){
    var card = el('div', {}, host);
    card.style.cssText = 'margin-top:36px; display:flex; align-items:center; gap:24px; padding:26px 40px; border-radius:22px;'
      + 'background:rgba(227,168,58,0.14); border:2px solid rgba(227,168,58,0.55);'
      + 'opacity:'+hp.op+'; transform:scale('+hp.sc+'); transform-origin:center;';
    var ic = el('div', {}, card); ic.style.cssText='font-size:56px;'; ic.textContent = '⚙️';
    var lab = el('div', {}, card); lab.style.cssText = 'font-size:36px; font-weight:800; color:#e3a83a; line-height:1.3;';
    lab.textContent = 'PTF: lo que Trabajo y Capital no explican';
  }
}

/* ================= SCENE 3: RESULTADO NACIONAL (Fig 2 — decomposición) ================= */
function renderDecompChart(localT, phaseStart){
  var lt = localT - phaseStart; // time since this chart started appearing
  var data = [
    {label:'Trabajo', v:AGG.laborAvg, color:'#3987e5', isPP:true},
    {label:'Capital', v:AGG.capitalAvg, color:'#199e70', isPP:true},
    {label:'PTF', v:AGG.ptfAvg, color:'#e66767', isPP:true},
    {label:'Valor agregado', v:AGG.vabAvg, color:'#9a9890', isPP:false}
  ];
  var w=1000,h=1200, marginL=30,marginR=30,marginT=60,marginB=170;
  var plotW=w-marginL-marginR, plotH=h-marginT-marginB;
  var minV=-0.8, maxV=4.0;
  function yScale(v){ return marginT+plotH-((v-minV)/(maxV-minV))*plotH; }
  var svg = svgEl('svg', {viewBox:'0 0 '+w+' '+h, width:'100%', height:'100%'}, null);
  [0,1,2,3,4].forEach(function(g){
    var y=yScale(g);
    svgEl('line',{x1:marginL,x2:w-marginR,y1:y,y2:y,stroke:'#2c2c2a','stroke-width':2}, svg);
    var t=svgEl('text',{x:marginL-2,y:y-8,'font-size':18,fill:'#8a8880'}, svg); t.textContent=g+'%';
  });
  var zeroY = yScale(0);
  svgEl('line',{x1:marginL,x2:w-marginR,y1:zeroY,y2:zeroY,stroke:'#3f3f3c','stroke-width':3}, svg);
  var bw = plotW/data.length;
  data.forEach(function(d,i){
    var cx = marginL+bw*i+bw/2, barW=bw*0.5;
    var growFrom = 200+i*450;
    var growP = easeOutCubic(smoothstep(growFrom, growFrom+650, lt));
    var y0=zeroY, y1=yScale(d.v);
    var curFar = y0+(y1-y0)*growP;
    var top=Math.min(y0,curFar), hgt=Math.max(0,Math.abs(curFar-y0));
    svgEl('rect',{x:cx-barW/2,y:top,width:barW,height:hgt,rx:8,fill:d.color,opacity:0.94}, svg);
    var lblOp = clamp((growP-0.55)/0.45,0,1);
    if (lblOp>0){
      var lbl = svgEl('text',{x:cx,y: d.v>=0?top-18:top+hgt+38,'text-anchor':'middle','font-size':32,'font-weight':800,fill:'#ffffff',opacity:lblOp}, svg);
      lbl.textContent = fmtPct(d.v,1);
    }
    var nmOp = smoothstep(growFrom-150,growFrom+150,lt);
    var nm = svgEl('text',{x:cx,y:h-118,'text-anchor':'middle','font-size':24,'font-weight':700,fill:'#c3c2b7',opacity:nmOp}, svg);
    nm.textContent = d.label;
  });
  var cap = svgEl('text',{x:w/2,y:h-40,'text-anchor':'middle','font-size':24,'font-weight':700,fill:'#c3c2b7',opacity:smoothstep(0,300,lt)}, svg);
  cap.textContent = 'De dónde vino el crecimiento (pp/año, 2005–2025)';
  return svg;
}
function renderS3(localT){
  var host = document.getElementById('s3-chart');
  clearEl(host);
  var d = renderDecompChart(localT, 0);
  d.style.position='absolute'; d.style.inset='0';
  host.appendChild(d);
}

/* ================= SCENE 4: QUE TAN IMPORTANTE ES (CONTRAFACTUAL) ================= */
function renderS4(localT){
  var host = document.getElementById('s4-body');
  clearEl(host);

  var subP = pop(localT, 200, 500);
  if (subP.op > 0.01){
    var sub = el('div', {}, host);
    sub.style.cssText = 'font-size:34px; color:#c3c2b7; font-weight:600; line-height:1.4; max-width:820px;'
      + 'opacity:'+subP.op+';';
    sub.textContent = 'Si la PTF hubiera crecido un punto porcentual más cada año…';
    host.appendChild(sub);
  }

  var numP = pop(localT, 1300, 900);
  if (numP.op > 0.01){
    var num = el('div', {}, host);
    num.className = 'serif';
    num.style.cssText = 'margin-top:36px; font-size:196px; font-weight:800; color:#e3a83a; line-height:1;'
      + 'opacity:'+numP.op+'; transform:scale('+numP.sc+'); transform-origin:center;';
    num.textContent = '+' + COUNTERFACTUAL_PP.toFixed(1).replace('.',',') + '%';
    host.appendChild(num);
  }

  var capP = pop(localT, 2600, 500);
  if (capP.op > 0.01){
    var cap = el('div', {}, host);
    cap.style.cssText = 'margin-top:18px; font-size:38px; color:#ffffff; font-weight:700; opacity:'+capP.op+';';
    cap.textContent = 'más producción en 2025';
    host.appendChild(cap);
  }

  var barsP = pop(localT, 3800, 600);
  if (barsP.op > 0.01){
    var wrap = el('div', {}, host);
    wrap.style.cssText = 'margin-top:80px; display:flex; gap:60px; align-items:flex-end; opacity:'+barsP.op+';';
    var maxH = 220;
    [{label:'Observado', v:1, color:'#6f6d66'}, {label:'Con +1pp de PTF/año', v:1+COUNTERFACTUAL_PP/100, color:'#e3a83a'}].forEach(function(d){
      var col = el('div', {}, wrap);
      col.style.cssText = 'display:flex; flex-direction:column; align-items:center; gap:16px;';
      var barH = maxH * (d.v/(1+COUNTERFACTUAL_PP/100));
      var bar = el('div', {}, col);
      bar.style.cssText = 'width:140px; height:'+barH+'px; border-radius:10px 10px 0 0; background:'+d.color+';';
      var lab = el('div', {}, col);
      lab.style.cssText = 'font-size:24px; color:#c3c2b7; font-weight:600; text-align:center; max-width:180px;';
      lab.textContent = d.label;
      col.appendChild(bar); col.appendChild(lab);
    });
    host.appendChild(wrap);
  }
}

/* ================= SCENE 5: SECTORES ================= */
// Sequential highlight matching the exact order the guion narrates the sectors in.
var S5_SEQUENCE = [
  {codes:['comercio','agro'],    from:3000,  to:6500},
  {codes:['mineria'],           from:6500,  to:8500},
  {codes:['financieras'],       from:8500,  to:10500},
  {codes:['construccion'],      from:10500, to:12500},
  {codes:['serv_publicos'],     from:12500, to:14000},
  {codes:['manufactura'],       from:14000, to:16000}
];
function phaseWeight(ph, t){
  return clamp(smoothstep(ph.from, ph.from+300, t) - smoothstep(ph.to-300, ph.to, t), 0, 1);
}
function renderS5(localT){
  var host = document.getElementById('s5-chart');
  clearEl(host);
  var data = SECTORS.slice().sort(function(a,b){ return b.cum-a.cum; });
  var w=1000, rowH=118, marginL=30, marginR=170, marginT=10, marginB=10;
  var plotW = w-marginL-marginR;
  var h = marginT+marginB+rowH*data.length;
  var maxAbs = Math.max.apply(null, data.map(function(d){return Math.abs(d.cum);}))*1.12;
  function xScale(v){ return (v/maxAbs)*(plotW/2); }
  var midX = marginL+plotW/2;
  var svg = svgEl('svg', {viewBox:'0 0 '+w+' '+h, width:'100%', height:'100%'}, host);
  svgEl('line',{x1:midX,x2:midX,y1:marginT,y2:h-marginB,stroke:'#3f3f3c','stroke-width':2}, svg);

  var anyW = 0;
  S5_SEQUENCE.forEach(function(ph){ anyW = Math.max(anyW, phaseWeight(ph, localT)); });

  data.forEach(function(d,i){
    var p = pop(localT, 300+i*160, 420);
    if (p.op<=0.01) return;
    var cy = marginT+rowH*i+rowH/2;
    var barW = xScale(Math.abs(d.cum))*p.sc;
    var isPos = d.cum>=0;
    var farX = isPos ? midX+barW : midX-barW;
    var ownW = 0;
    S5_SEQUENCE.forEach(function(ph){ if (ph.codes.indexOf(d.code)>=0) ownW = Math.max(ownW, phaseWeight(ph, localT)); });
    var dimW = clamp(anyW - ownW, 0, 1);
    var glow = ownW > 0.05;
    var col = isPos ? '#3987e5' : '#e66767';
    var op = p.op * (1 - 0.68*dimW);
    var rect = svgEl('rect', {x:Math.min(midX,farX), y:cy-26, width:Math.max(1,barW), height:52, rx:8, fill:col, opacity:op}, svg);
    if (glow){
      svgEl('rect', {x:Math.min(midX,farX)-4, y:cy-30, width:Math.max(1,barW)+8, height:60, rx:10, fill:'none', stroke:'#e3a83a','stroke-width':4, opacity:ownW*0.9}, svg);
    }
    var nm = svgEl('text', {x:marginL, y:cy-36, 'font-size':24, 'font-weight':700, fill: glow?'#e3a83a':'#c3c2b7', opacity:p.op*(1-0.6*dimW)}, svg);
    nm.textContent = d.name;
    // Long bars can push an outside label past the canvas edge — fall back to
    // an inside-the-bar placement near its tip when there isn't room outside.
    var valX, valAnchor;
    if (isPos){
      if (w-marginR-farX < 130){ valX = farX-14; valAnchor='end'; }
      else { valX = farX+14; valAnchor='start'; }
    } else {
      if (farX < 150){ valX = farX+14; valAnchor='start'; }
      else { valX = farX-14; valAnchor='end'; }
    }
    var val = svgEl('text', {x:valX, y:cy+9, 'text-anchor':valAnchor, 'font-size':30, 'font-weight':800, fill:'#ffffff', opacity:op}, svg);
    val.textContent = fmtPct(d.cum,1);
  });
}

/* ================= SCENE 6: CIERRE ================= */
function renderS6(localT){
  var endc = document.getElementById('s6-end');
  var endOp = clamp(smoothstep(0,700,localT), 0, 1);
  endc.style.display = endOp>0.01 ? 'flex' : 'none';
  endc.style.opacity = endOp;
  endc.style.transform = 'scale(' + (0.94+0.06*endOp) + ')';
}

/* ================= DISPATCH ================= */
var RENDERERS = {s1:renderS1, s2:renderS2, s3:renderS3, s4:renderS4, s5:renderS5, s6:renderS6};

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
  var envelope = smoothstep(0,260,localT) * (1 - smoothstep(dur-220,dur,localT));
  var sceneEl = document.getElementById(active.id);
  sceneEl.style.opacity = Math.max(0.001, envelope);
  RENDERERS[active.id](localT);
};

// Auto-invoke on load using the time encoded in the URL hash (#t=12345)
var m = /t=(\d+)/.exec(location.hash);
window.__renderFrame(m ? parseInt(m[1],10) : 0);

})();
