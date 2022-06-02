  /*
  This is adapted from the compiled version of the custom element
  for Katex originally written in TypeScript, available here:
  https://github.com/justinfagnani/katex-elements
  */
  function initKatex() {
  
    const supportsAdoptingStyleSheets = ('adoptedStyleSheets' in Document.prototype) &&
    ('replace' in CSSStyleSheet.prototype);
let styleSheet;
const getStyleSheet = () => {
    if (styleSheet === undefined) {
        styleSheet = new CSSStyleSheet();
        styleSheet.replace(styles);
    }
    return styleSheet;
};
const template = document.createElement('template');
template.innerHTML = `
  ${!supportsAdoptingStyleSheets ? `<style>${styles}</style>` : ''}
  <style>
    :host {
      display: inline-block;
    }
    * {
      user-select:none;
    }
  </style>
  <div id="container"></div>
  <div hidden><slot></slot></div>
`;
class KatexExprElement extends HTMLElement {
    constructor() {
        super();
        const shadowRoot = this.attachShadow({ mode: 'open' });
        shadowRoot.appendChild(document.importNode(template.content, true));
        if (supportsAdoptingStyleSheets) {
            shadowRoot.adoptedStyleSheets = [getStyleSheet()];
        }
        this._container = shadowRoot.querySelector('#container');
        this._slot = shadowRoot.querySelector('slot');
        this._slot.addEventListener('slotchange', () => this._render());
        // slotchange is not enough
        observer =  new MutationObserver( () => this._render());
        observer.observe(this, { characterData : true, subtree : true }); 
        this._styleTag = shadowRoot.querySelector('style');
    }
    get macros() {
        return this._macros;
    }
    set macros(v) {
        if (v == null) {
            this.removeAttribute('macros');
        }
        else {
            try {
                const json = JSON.stringify(v);
                this._macros = v;
                this.setAttribute('macros', json);
            }
            catch (e) {
                this._macros = undefined;
                this.removeAttribute('macros');
                throw e;
            }
        }
    }
    /**
     * The Katex displayMode:
     *
     * If true, math will be rendered in display mode (math in display style and
     * center math on page)
     *
     * If false, math will be rendered in inline mode
     */
    get displayMode() {
        return this.hasAttribute('display-mode');
    }
    set displayMode(v) {
        if (v) {
            this.setAttribute('display-mode', '');
        }
        else {
            this.removeAttribute('display-mode');
        }
    }
    get leqno() {
        return this.hasAttribute('leqno');
    }
    set leqno(v) {
        if (v) {
            this.setAttribute('leqno', '');
        }
        else {
            this.removeAttribute('leqno');
        }
    }
    get fleqn() {
        return this.hasAttribute('fleqn');
    }
    set fleqn(v) {
        if (v) {
            this.setAttribute('fleqn', '');
        }
        else {
            this.removeAttribute('fleqn');
        }
    }
    /**
     * Specifies a minimum thickness, in ems, for fraction lines, \sqrt top lines,
     * {array} vertical lines, \hline, \hdashline, \underline, \overline, and the
     * borders of \fbox, \boxed, and \fcolorbox. The usual value for these items
     * is 0.04, so for minRuleThickness to be effective it should probably take a
     * value slightly above 0.04, say 0.05 or 0.06. Negative values will be
     * ignored.
     */
    get minRuleThickness() {
        const attrValue = this.getAttribute('min-rule-thickness');
        if (attrValue == null) {
            return undefined;
        }
        return parseFloat(attrValue);
    }
    set minRuleThickness(v) {
        if (v == null) {
            this.removeAttribute('min-rule-thickness');
        }
        else {
            this.setAttribute('min-rule-thickness', String(v));
        }
    }
    attributeChangedCallback(_name) {
        this._render();
    }
    _render() {
        const hostRule = this._styleTag.sheet.cssRules[0];
        hostRule.style.display = this.displayMode ? 'block' : 'inline-block';
        const inputText = this._slot.assignedNodes({ flatten: true }).map((n) => n.textContent).join('');
        katex.render(inputText, this._container, 
              { displayMode: this.displayMode, throwOnError : false });
        // Notify that it is rendered
        dispatchRendered(this);
    }
}
KatexExprElement.observedAttributes = ['display-mode', 'leqno', 'fleqn', 'macros'];
customElements.define('math-latex', KatexExprElement);


  
  }