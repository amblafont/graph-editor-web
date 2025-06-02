  /*
  This is adapted from the compiled version of the custom element
  for Katex originally written in TypeScript, available here:
  https://github.com/justinfagnani/katex-elements
  */
  function initKatex() {
    const supportsAdoptingStyleSheets = ('adoptedStyleSheets' in Document.prototype) &&
    ('replace' in CSSStyleSheet.prototype);
let styleSheet = new CSSStyleSheet();
styleSheet.replace(katexStyles);
document.adoptedStyleSheets = [styleSheet];


  
    const template = document.createElement('template');
    // I tried inserting this link tag in the template, but it did not work
    // very well (I had some glitches)
    // So, I sticked to the adoptingStyleSheets methods, although
    // it requires to turn the katex stylesheet file into a js string
    // (check the makefile)
    // <link rel="stylesheet" href="katex/katex.min.css">
    template.innerHTML = `
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
              shadowRoot.adoptedStyleSheets = [styleSheet];
          }  
            this._container = shadowRoot.querySelector('#container');
            this._slot = shadowRoot.querySelector('slot');
            this._slot.addEventListener('slotchange', () => this._render());
            // slotchange is not enough
            observer =  new MutationObserver( () => this._render());
            observer.observe(this, { characterData : true, subtree : true }); 
            // This attributeObserver is useful when
            // data-height or data-width are updated to check
            // that they are indeed correct 
            // (the Elm command computeLayout runs before rendering the model, so it does not help!)
            var attributeObserver = new MutationObserver( () => dispatchRendered(this));
            attributeObserver.observe(this, { attributes : true }); 
        }
    
        _render() {
            const inputText = this._slot.assignedNodes({ flatten: true }).map((n) => n.textContent).join('');
            katex.render(inputText, this._container, 
                { throwOnError : false });
            // Notify that it is rendered
            dispatchRendered(this);
        }
    }
    customElements.define('math-latex', KatexExprElement);


  
}