// timeline_d3.js
// D3 + SVG timeline: replace canvas with SVG; create on background rect only, edit on event elements only.

(function() {
  'use strict';

  const DOMAINS = ['observation_period', 'visit_occurrence', 'visit_detail', 'drug_exposure', 'condition_occurrence', 'procedure_occurrence', 'measurement'];
  const POINT_DOMAINS = ['procedure_occurrence', 'measurement'];
  const ROW_HEIGHT = 40;
  const PADDING_LEFT = 80;
  const PADDING_RIGHT = 20;
  const PADDING_TOP = 25;
  const HANDLE_RADIUS = 6;
  const BODY_HEIGHT = 8;
  const LABEL_OFFSET = 18;
  const MOVE_THRESHOLD_PX = 5;
  const DOMAIN_COLORS = {
    observation_period: '#64748b',
    visit_occurrence: '#007bff',
    visit_detail: '#6c757d',
    drug_exposure: '#28a745',
    condition_occurrence: '#dc3545',
    procedure_occurrence: '#ffc107',
    measurement: '#17a2b8'
  };

  function isPointDomain(domain) {
    return POINT_DOMAINS.indexOf(domain) !== -1;
  }

  function parseLocalDate(dateStr) {
    if (!dateStr) return null;
    const parts = String(dateStr).split('-');
    if (parts.length !== 3) return null;
    return new Date(parseInt(parts[0], 10), parseInt(parts[1], 10) - 1, parseInt(parts[2], 10));
  }

  function formatLocalDate(date) {
    if (!date || !(date instanceof Date) || isNaN(date.getTime())) return null;
    const y = date.getFullYear();
    const m = String(date.getMonth() + 1).padStart(2, '0');
    const d = String(date.getDate()).padStart(2, '0');
    return `${y}-${m}-${d}`;
  }

  function clampDate(dateStr, minDate, maxDate) {
    const d = parseLocalDate(dateStr);
    if (!d) return null;
    const min = minDate ? parseLocalDate(minDate) : null;
    const max = maxDate ? parseLocalDate(maxDate) : null;
    let t = d.getTime();
    if (min && t < min.getTime()) t = min.getTime();
    if (max && t > max.getTime()) t = max.getTime();
    return formatLocalDate(new Date(t));
  }

  function calculateAge(dobStr, targetDateStr) {
    if (!dobStr || !targetDateStr) return null;
    try {
      const dob = new Date(dobStr + 'T00:00:00');
      const target = new Date(targetDateStr + 'T00:00:00');
      if (isNaN(dob.getTime()) || isNaN(target.getTime())) return null;
      let age = target.getFullYear() - dob.getFullYear();
      const monthDiff = target.getMonth() - dob.getMonth();
      if (monthDiff < 0 || (monthDiff === 0 && target.getDate() < dob.getDate())) age--;
      return Math.floor(age);
    } catch (e) {
      return null;
    }
  }

  function initTimeline(svgEl) {
    if (svgEl.getAttribute('data-timeline-initialized') === 'true') return null;
    const id = svgEl.id;
    const ns = id.replace(/-timeline-svg$/, '');
    console.log('initTimeline: svgEl.id=', id, 'ns=', ns);
    if (!ns || ns === id) return null;
    svgEl.setAttribute('data-timeline-initialized', 'true');

    const container = svgEl.closest('.timeline-container') || svgEl.parentElement;
    const width = container ? container.getBoundingClientRect().width : 800;
    const height = container ? container.getBoundingClientRect().height : 320;

    const svg = d3.select(svgEl);
    svg.attr('width', width).attr('height', height).attr('viewBox', `0 0 ${width} ${height}`);

    const grid = svg.append('g').attr('class', 'grid');
    const interactionLayer = svg.append('g').attr('class', 'timeline-interaction');
    const bgRect = interactionLayer.append('rect')
      .attr('class', 'bg-hit')
      .attr('x', 0)
      .attr('y', 0)
      .attr('width', width)
      .attr('height', height)
      .attr('fill', 'transparent')
      .style('pointer-events', 'all');
    const eventsG = interactionLayer.append('g').attr('class', 'events');
    const overlay = svg.append('g').attr('class', 'overlay');

    const tooltipEl = document.createElement('div');
    tooltipEl.style.cssText = 'position: fixed; background: rgba(0, 0, 0, 0.85); color: white; padding: 4px 8px; border-radius: 4px; font-size: 11px; pointer-events: none; z-index: 10000; display: none; font-family: Arial, sans-serif; white-space: nowrap; box-shadow: 0 2px 4px rgba(0,0,0,0.3);';
    document.body.appendChild(tooltipEl);

    const state = {
      ns,
      svg,
      grid,
      interactionLayer,
      eventsG,
      overlay,
      container,
      tooltipEl,
      events: [],
      startDate: null,
      endDate: null,
      dateOfBirth: null,
      width,
      height,
      xScale: null,
      rowY: {}
    };

    function svgPoint(svgNode, clientX, clientY) {
      const pt = svgNode.createSVGPoint();
      pt.x = clientX;
      pt.y = clientY;
      const ctm = svgNode.getScreenCTM();
      return ctm ? pt.matrixTransform(ctm.inverse()) : { x: pt.x, y: pt.y };
    }

    function showHoverTooltip(state, event) {
      if (!state.xScale) return;
      const pt = svgPoint(state.svg.node(), event.clientX, event.clientY);
      const x = pt.x;
      if (x < PADDING_LEFT || x > state.width - PADDING_RIGHT) {
        state.tooltipEl.style.display = 'none';
        return;
      }
      const dateStr = formatLocalDate(state.xScale.invert(x));
      if (!dateStr) {
        state.tooltipEl.style.display = 'none';
        return;
      }
      let text = dateStr;
      if (state.dateOfBirth) {
        const age = calculateAge(state.dateOfBirth, dateStr);
        if (age !== null && !isNaN(age)) {
          const ageText = age >= 0 ? 'Age: ' + age : 'Age: ' + age + ' (not yet born)';
          text = dateStr + ' \u2022 ' + ageText;
        }
      }
      state.tooltipEl.textContent = text;
      state.tooltipEl.style.display = 'block';
      state.tooltipEl.style.left = (event.pageX + 10) + 'px';
      state.tooltipEl.style.top = (event.pageY - 30) + 'px';
    }

    function hideTooltip(state) {
      if (state.tooltipEl) state.tooltipEl.style.display = 'none';
    }

    svg.on('mousemove', function(event) {
      showHoverTooltip(state, event);
    });
    svg.on('mouseleave', function() {
      hideTooltip(state);
    });

    // Compute row Y positions
    function computeRowY() {
      const h = state.height;
      const rows = DOMAINS.length;
      const chartTop = PADDING_TOP;
      state.rowY = {};
      DOMAINS.forEach((domain, i) => {
        state.rowY[domain] = chartTop + i * ROW_HEIGHT;
      });
    }

    computeRowY();

    // Create drag: only on background rect
    const createDrag = d3.drag()
      .on('start', function(event) {
        event.sourceEvent.stopPropagation();
        event.sourceEvent.preventDefault();
        const x = event.x;
        const y = event.y;
        const domain = domainAtY(y);
        const minDate = state.xScale ? state.xScale.domain()[0] : null;
        const maxDate = state.xScale ? state.xScale.domain()[1] : null;
        const startDate = minDate && maxDate ? formatLocalDate(state.xScale.invert(x)) : null;
        if (!startDate || !domain) return;
        state.create = {
          startX: x,
          startY: y,
          startDate,
          domain,
          endX: x,
          endDate: startDate,
          previewLine: null
        };
        console.log('CREATE start', state.create.domain, state.create.startDate);
      })
      .on('drag', function(event) {
        if (!state.create) return;
        state.create.endX = Math.max(0, Math.min(state.width, event.x));
        state.create.endY = event.y;
        const domain = domainAtY(state.create.endY);
        if (domain) state.create.domain = domain;
        if (state.xScale) {
          state.create.endDate = formatLocalDate(state.xScale.invert(state.create.endX));
          if (!state.create.endDate) state.create.endDate = state.create.startDate;
        }
        renderCreatePreview();
      })
      .on('end', function(event) {
        if (!state.create) return;
        const cr = state.create;
        const dx = Math.abs(cr.endX - cr.startX);
        const dy = Math.abs(cr.endY - cr.startY);
        const moved = dx >= MOVE_THRESHOLD_PX || dy >= MOVE_THRESHOLD_PX;

        let finalStart = cr.startDate;
        let finalEnd = cr.endDate;
        if (isPointDomain(cr.domain)) {
          if (moved) {
            finalStart = cr.endDate || cr.startDate;
            finalEnd = finalStart;
          } else {
            finalEnd = finalStart;
          }
        } else {
          if (!moved) {
            finalEnd = finalStart;
          } else {
            if (finalEnd < finalStart) {
              const t = finalStart;
              finalStart = finalEnd;
              finalEnd = t;
            }
          }
        }

        const payload = {
          gesture_type: 'create',
          domain: cr.domain,
          start_date: finalStart,
          end_date: finalEnd,
          drag_start_x: cr.startX,
          nonce: Date.now() + '_' + Math.random()
        };
        const createInputName = state.ns + '-create_event';
        console.log('setInputValue create', createInputName, payload);
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue(createInputName, payload, { priority: 'event' });
        }
        removeCreatePreview();
        state.create = null;
      });

    interactionLayer.select('rect.bg-hit').call(createDrag);

    function domainAtY(y) {
      let closest = null;
      let minD = Infinity;
      DOMAINS.forEach(domain => {
        const cy = state.rowY[domain];
        const d = Math.abs(y - cy);
        if (d < minD) {
          minD = d;
          closest = domain;
        }
      });
      return closest;
    }

    function renderCreatePreview() {
      state.eventsG.selectAll('.create-preview').remove();
      if (!state.create || !state.xScale) return;
      const cr = state.create;
      const y = state.rowY[cr.domain];
      const x1 = state.xScale(parseLocalDate(cr.startDate)) || cr.startX;
      const x2 = state.xScale(parseLocalDate(cr.endDate)) || cr.endX;
      const color = DOMAIN_COLORS[cr.domain] || '#000';
      const g = state.eventsG.append('g').attr('class', 'create-preview');
      g.append('line')
        .attr('x1', x1)
        .attr('x2', x2)
        .attr('y1', y)
        .attr('y2', y)
        .attr('stroke', color)
        .attr('stroke-width', 3);
      g.append('circle').attr('class', 'handle start').attr('cx', x1).attr('cy', y).attr('r', HANDLE_RADIUS).attr('fill', '#fff').attr('stroke', color).attr('stroke-width', 2);
      g.append('circle').attr('class', 'handle end').attr('cx', x2).attr('cy', y).attr('r', HANDLE_RADIUS).attr('fill', '#fff').attr('stroke', color).attr('stroke-width', 2);
    }

    function removeCreatePreview() {
      state.eventsG.selectAll('.create-preview').remove();
    }

    state.domainAtY = domainAtY;
    state.renderCreatePreview = renderCreatePreview;
    state.removeCreatePreview = removeCreatePreview;
    state.computeRowY = computeRowY;
    return state;
  }

  function renderTimeline(state, events, config) {
    if (!state || !state.svg) return;
    state.events = events || [];
    state.startDate = config.start_date || null;
    state.endDate = config.end_date || null;
    state.dateOfBirth = config.date_of_birth || null;

    const container = state.container || state.svg.node().parentElement;
    const width = container ? container.getBoundingClientRect().width : 800;
    const height = container ? container.getBoundingClientRect().height : 320;
    state.width = width;
    state.height = height;
    state.svg.attr('width', width).attr('height', height).attr('viewBox', `0 0 ${width} ${height}`);

    state.computeRowY();

    let minDate = config.start_date ? parseLocalDate(config.start_date) : new Date();
    let maxDate = config.end_date ? parseLocalDate(config.end_date) : new Date();
    if (!minDate || isNaN(minDate.getTime())) minDate = new Date();
    if (!maxDate || isNaN(maxDate.getTime())) maxDate = new Date();
    if (maxDate <= minDate) maxDate = new Date(minDate.getTime() + 86400000);

    const xScale = d3.scaleTime()
      .domain([minDate, maxDate])
      .range([PADDING_LEFT, width - PADDING_RIGHT]);
    state.xScale = xScale;

    state.interactionLayer.select('rect.bg-hit').attr('width', width).attr('height', height);

    // Grid: domain rows and labels
    state.grid.selectAll('*').remove();
    DOMAINS.forEach(domain => {
      const y = state.rowY[domain];
      state.grid.append('line')
        .attr('x1', 0)
        .attr('x2', width)
        .attr('y1', y)
        .attr('y2', y)
        .attr('stroke', '#e0e0e0')
        .attr('stroke-width', 1);
      state.grid.append('text')
        .attr('x', 5)
        .attr('y', y - 5)
        .attr('fill', '#666')
        .attr('font-size', '11px')
        .attr('font-family', 'Arial')
        .text(domain.replace('_', ' '));
    });
    const measurementY = state.rowY['measurement'];
    for (let i = 0; i <= 10; i++) {
      const x = PADDING_LEFT + (width - PADDING_LEFT - PADDING_RIGHT) * (i / 10);
      state.grid.append('line')
        .attr('x1', x)
        .attr('x2', x)
        .attr('y1', 0)
        .attr('y2', measurementY)
        .attr('stroke', '#e0e0e0')
        .attr('stroke-width', 1);
    }
    const labelY = measurementY + 22;
    if (labelY < height) {
      for (let i = 1; i < 10; i++) {
        const x = PADDING_LEFT + (width - PADDING_LEFT - PADDING_RIGHT) * (i / 10);
        const t = minDate.getTime() + (maxDate - minDate) * (i / 10);
        const label = formatLocalDate(new Date(t));
        if (label) {
          state.grid.append('text')
            .attr('x', x)
            .attr('y', labelY)
            .attr('text-anchor', 'middle')
            .attr('fill', '#999')
            .attr('font-size', '12px')
            .attr('font-family', 'Arial')
            .text(label);
        }
      }
    }

    // Data join: events
    const key = d => `${d.domain}:${d.id_value}`;
    const eventSel = state.eventsG.selectAll('g.event').data(state.events.filter(e => e && e.domain && e.id_value != null), key);

    eventSel.exit().remove();

    const enter = eventSel.enter().append('g').attr('class', 'event');

    enter.each(function(d) {
      const g = d3.select(this);
      const y = state.rowY[d.domain] || PADDING_TOP;
      const color = DOMAIN_COLORS[d.domain] || '#000';
      const pointDomain = isPointDomain(d.domain);

      if (pointDomain) {
        const circle = g.append('circle')
          .attr('class', 'point')
          .attr('r', HANDLE_RADIUS)
          .attr('fill', color)
          .attr('stroke', '#fff')
          .attr('stroke-width', 2)
          .style('cursor', 'grab');
        attachPointDrag(state, circle, d);
      } else {
        const body = g.append('line')
          .attr('class', 'body')
          .attr('stroke', color)
          .attr('stroke-width', 3)
          .style('cursor', 'move');
        const startHandle = g.append('circle')
          .attr('class', 'handle start')
          .attr('r', HANDLE_RADIUS)
          .attr('fill', '#fff')
          .attr('stroke', color)
          .attr('stroke-width', 2)
          .style('cursor', 'ew-resize');
        const endHandle = g.append('circle')
          .attr('class', 'handle end')
          .attr('r', HANDLE_RADIUS)
          .attr('fill', '#fff')
          .attr('stroke', color)
          .attr('stroke-width', 2)
          .style('cursor', 'ew-resize');
        attachIntervalDrags(state, body, startHandle, endHandle, d);
      }
      g.append('text')
        .attr('class', 'event-label')
        .attr('text-anchor', 'middle')
        .attr('dominant-baseline', 'text-before-edge')
        .attr('font-size', '11px')
        .attr('font-family', 'Arial')
        .attr('fill', '#000')
        .attr('pointer-events', 'none')
        .text('ID: ' + (d.concept_id != null ? d.concept_id : ''));
    });


    const merged = enter.merge(eventSel);

    merged.each(function(d) {
      const g = d3.select(this);
      const y = state.rowY[d.domain] || PADDING_TOP;
      const startDate = parseLocalDate(d.start_date);
      const endDate = parseLocalDate(d.end_date || d.start_date);
      const x1 = startDate ? state.xScale(startDate) : PADDING_LEFT;
      const x2 = endDate ? state.xScale(endDate) : x1;
      const pointDomain = isPointDomain(d.domain);
      if (pointDomain) {
        g.select('circle.point').attr('cx', x1).attr('cy', y);
      } else {
        g.select('line.body').attr('x1', x1).attr('x2', x2).attr('y1', y).attr('y2', y);
        g.select('circle.handle.start').attr('cx', x1).attr('cy', y);
        g.select('circle.handle.end').attr('cx', x2).attr('cy', y);
      }
      const labelX = pointDomain ? x1 : (x1 + x2) / 2;
      g.select('text.event-label')
        .attr('x', labelX)
        .attr('y', y - LABEL_OFFSET)
        .attr('dominant-baseline', 'text-before-edge')
        .text('ID: ' + (d.concept_id != null ? d.concept_id : ''));
    });

    renderCreatePreview.call(state);
  }

  function clampToScale(dateStr, state) {
    if (!state.xScale) return dateStr;
    const d = parseLocalDate(dateStr);
    if (!d) return dateStr;
    const domain = state.xScale.domain();
    let t = d.getTime();
    if (domain[0] && t < domain[0].getTime()) t = domain[0].getTime();
    if (domain[1] && t > domain[1].getTime()) t = domain[1].getTime();
    return formatLocalDate(new Date(t));
  }

  function attachPointDrag(state, circleSel, d) {
    const drag = d3.drag()
      .on('start', function(event) {
        event.sourceEvent.stopPropagation();
        event.sourceEvent.preventDefault();
        d3.select(this).style('cursor', 'grabbing');
      })
      .on('drag', function(event) {
        const x = Math.max(PADDING_LEFT, Math.min(state.width - PADDING_RIGHT, event.x));
        const newDate = state.xScale ? formatLocalDate(state.xScale.invert(x)) : null;
        if (newDate) {
          d.start_date = newDate;
          d.end_date = newDate;
          circleSel.attr('cx', state.xScale(parseLocalDate(newDate))).attr('cy', state.rowY[d.domain]);
        }
      })
      .on('end', function(event) {
        d3.select(this).style('cursor', 'grab');
        const start = clampToScale(d.start_date, state);
        const end = d.end_date || start;
        const payload = {
          gesture_type: 'edit',
          domain: d.domain,
          id_value: d.id_value,
          start_date: start,
          end_date: end,
          nonce: Date.now() + '_' + Math.random()
        };
        console.log('EDIT end (point) – SENT update_event', payload);
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue(state.ns + '-update_event', payload, { priority: 'event' });
        }
      });
    circleSel.call(drag);
  }

  function attachIntervalDrags(state, bodySel, startHandleSel, endHandleSel, d) {
    const startDrag = d3.drag()
      .on('start', function(event) {
        event.sourceEvent.stopPropagation();
        event.sourceEvent.preventDefault();
      })
      .on('drag', function(event) {
        const x = Math.max(PADDING_LEFT, Math.min(state.width - PADDING_RIGHT, event.x));
        const newStart = state.xScale ? formatLocalDate(state.xScale.invert(x)) : null;
        if (!newStart) return;
        const end = d.end_date || d.start_date;
        if (newStart > end) {
          d.start_date = end;
          d.end_date = newStart;
        } else {
          d.start_date = newStart;
        }
        const x1 = state.xScale(parseLocalDate(d.start_date));
        const x2 = state.xScale(parseLocalDate(d.end_date || d.start_date));
        bodySel.attr('x1', x1).attr('x2', x2);
        startHandleSel.attr('cx', x1);
        endHandleSel.attr('cx', x2);
      })
      .on('end', function() {
        let start = clampToScale(d.start_date, state);
        let end = clampToScale(d.end_date || d.start_date, state);
        if (end < start) {
          const t = start;
          start = end;
          end = t;
        }
        const payload = {
          gesture_type: 'edit',
          domain: d.domain,
          id_value: d.id_value,
          start_date: start,
          end_date: end,
          nonce: Date.now() + '_' + Math.random()
        };
        console.log('EDIT end (start handle) – SENT update_event', payload);
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue(state.ns + '-update_event', payload, { priority: 'event' });
        }
      });

    const endDrag = d3.drag()
      .on('start', function(event) {
        event.sourceEvent.stopPropagation();
        event.sourceEvent.preventDefault();
      })
      .on('drag', function(event) {
        const x = Math.max(PADDING_LEFT, Math.min(state.width - PADDING_RIGHT, event.x));
        const newEnd = state.xScale ? formatLocalDate(state.xScale.invert(x)) : null;
        if (!newEnd) return;
        const start = d.start_date;
        if (newEnd < start) {
          d.end_date = start;
          d.start_date = newEnd;
        } else {
          d.end_date = newEnd;
        }
        const x1 = state.xScale(parseLocalDate(d.start_date));
        const x2 = state.xScale(parseLocalDate(d.end_date || d.start_date));
        bodySel.attr('x1', x1).attr('x2', x2);
        startHandleSel.attr('cx', x1);
        endHandleSel.attr('cx', x2);
      })
      .on('end', function() {
        let start = clampToScale(d.start_date, state);
        let end = clampToScale(d.end_date || d.start_date, state);
        if (end < start) {
          const t = start;
          start = end;
          end = t;
        }
        const payload = {
          gesture_type: 'edit',
          domain: d.domain,
          id_value: d.id_value,
          start_date: start,
          end_date: end,
          nonce: Date.now() + '_' + Math.random()
        };
        console.log('EDIT end (end handle) – SENT update_event', payload);
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue(state.ns + '-update_event', payload, { priority: 'event' });
        }
      });

    const bodyDrag = d3.drag()
      .on('start', function(event) {
        event.sourceEvent.stopPropagation();
        event.sourceEvent.preventDefault();
        state.bodyDragStart = {
          origStart: d.start_date,
          origEnd: d.end_date || d.start_date,
          startMouseX: event.x
        };
      })
      .on('drag', function(event) {
        const st = state.bodyDragStart;
        if (!st) return;
        const deltaX = event.x - st.startMouseX;
        const origStartD = parseLocalDate(st.origStart);
        const origEndD = parseLocalDate(st.origEnd);
        if (!origStartD || !origEndD || !state.xScale) return;
        const origStartX = state.xScale(origStartD);
        const newStartX = origStartX + deltaX;
        const newStartD = state.xScale.invert(newStartX);
        const dayMs = 86400000;
        const shiftMs = newStartD.getTime() - origStartD.getTime();
        const newEndD = new Date(origEndD.getTime() + shiftMs);
        d.start_date = formatLocalDate(newStartD);
        d.end_date = formatLocalDate(newEndD);
        const x1 = state.xScale(newStartD);
        const x2 = state.xScale(newEndD);
        bodySel.attr('x1', x1).attr('x2', x2);
        startHandleSel.attr('cx', x1);
        endHandleSel.attr('cx', x2);
      })
      .on('end', function() {
        let start = clampToScale(d.start_date, state);
        let end = clampToScale(d.end_date || d.start_date, state);
        if (end < start) {
          const t = start;
          start = end;
          end = t;
        }
        const payload = {
          gesture_type: 'edit',
          domain: d.domain,
          id_value: d.id_value,
          start_date: start,
          end_date: end,
          nonce: Date.now() + '_' + Math.random()
        };
        console.log('EDIT end (body) – SENT update_event', payload);
        if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
          Shiny.setInputValue(state.ns + '-update_event', payload, { priority: 'event' });
        }
        state.bodyDragStart = null;
      });

    startHandleSel.call(startDrag);
    endHandleSel.call(endDrag);
    bodySel.call(bodyDrag);
  }

  const timelineStates = {};

  function resizeAll() {
    Object.keys(timelineStates).forEach(function(ns) {
      const state = timelineStates[ns];
      if (state && state.events) {
        renderTimeline(state, state.events, {
          start_date: state.startDate,
          end_date: state.endDate,
          date_of_birth: state.dateOfBirth
        });
      }
    });
  }

  function ensureInit() {
    const svgs = document.querySelectorAll('[id$="-timeline-svg"]');
    svgs.forEach(svgEl => {
      const state = initTimeline(svgEl);
      if (state) {
        timelineStates[state.ns] = state;
        const handlerName = state.ns + '-update_timeline';
        if (typeof Shiny !== 'undefined' && Shiny.addCustomMessageHandler) {
          Shiny.addCustomMessageHandler(handlerName, function(data) {
            const events = data.events || [];
            const config = {
              start_date: data.start_date,
              end_date: data.end_date,
              date_of_birth: data.date_of_birth || null
            };
            renderTimeline(state, events, config);
          });
          Shiny.setInputValue(state.ns + '-timeline_ready', { ts: Date.now() }, { priority: 'event' });
        }
      }
    });
  }

  $(document).on('shiny:connected', function() {
    setTimeout(ensureInit, 100);
    setTimeout(ensureInit, 500);
  });
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function() {
      setTimeout(ensureInit, 300);
      setTimeout(ensureInit, 800);
    });
  } else {
    setTimeout(ensureInit, 300);
    setTimeout(ensureInit, 800);
  }

  $(document).on('shiny:value', function() {
    setTimeout(ensureInit, 200);
  });

  window.addEventListener('resize', function() {
    resizeAll();
  });
})();
