// Custom Web Component for Nominatim Map with Reverse Geocoding
class NominatimReverseGeocoding extends HTMLElement {
  constructor() {
    super();
    this.map = null;
    this.marker = null;
    this.lat = parseFloat(this.getAttribute('lat')) || 19.4326;
    this.lng = parseFloat(this.getAttribute('lng')) || -99.1332;
  }

  connectedCallback() {
    const container = document.createElement('div');
    container.style.width = '100%';
    container.style.height = '400px';
    container.style.border = '1px solid #d1d1d1';
    container.style.borderRadius = '4px';
    this.appendChild(container);

    // Initialize Leaflet map
    this.map = L.map(container).setView([this.lat, this.lng], 13);

    // Add OpenStreetMap tile layer
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
      attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
      maxZoom: 19
    }).addTo(this.map);

    // Add initial marker if coordinates are explicitly set via attributes
    const hasExplicitCoords = this.hasAttribute('lat') && this.hasAttribute('lng');
    if (hasExplicitCoords) {
      this.marker = L.marker([this.lat, this.lng]).addTo(this.map);
    }

    // Handle map clicks
    this.map.on('click', async (e) => {
      const { lat, lng } = e.latlng;

      // Update or create marker
      if (this.marker) {
        this.marker.setLatLng([lat, lng]);
      } else {
        this.marker = L.marker([lat, lng]).addTo(this.map);
      }

      // Reverse geocode using Nominatim
      try {
        const response = await fetch(
          `https://nominatim.openstreetmap.org/reverse?format=json&lat=${lat}&lon=${lng}&addressdetails=1`,
          {
            headers: {
              'User-Agent': 'ElmFormToolkitDemo'
            }
          }
        );
        const data = await response.json();

        if (data && data.address) {
          // Dispatch custom event with address data
          this.dispatchEvent(new CustomEvent('address-selected', {
            detail: {
              lat: lat,
              lng: lng,
              address: data.address,
              displayName: data.display_name
            },
            bubbles: true,
            composed: true
          }));
        }
      } catch (error) {
        console.error('Geocoding error:', error);
      }
    });
  }

  disconnectedCallback() {
    if (this.map) {
      this.map.remove();
    }
  }

  static get observedAttributes() {
    return ['lat', 'lng'];
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (!this.map) return;

    if (name === 'lat' || name === 'lng') {
      const lat = parseFloat(this.getAttribute('lat')) || this.lat;
      const lng = parseFloat(this.getAttribute('lng')) || this.lng;

      this.map.setView([lat, lng], 13);

      if (this.marker) {
        this.marker.setLatLng([lat, lng]);
      } else {
        this.marker = L.marker([lat, lng]).addTo(this.map);
      }
    }
  }
}

// Register the custom element
customElements.define('nominatim-reverse-geocoding', NominatimReverseGeocoding);


// Custom Web Component for Choices.js Multi-Select
class ChoicesMultiSelect extends HTMLElement {
  constructor() {
    super();
    this.choices = null;
  }

  connectedCallback() {
    // Create select element
    const select = document.createElement('select');
    select.setAttribute('multiple', '');

    // Add some default options
    const options = [
      { value: 'javascript', label: 'JavaScript' },
      { value: 'python', label: 'Python' },
      { value: 'elm', label: 'Elm' },
      { value: 'rust', label: 'Rust' },
      { value: 'go', label: 'Go' },
      { value: 'typescript', label: 'TypeScript' },
      { value: 'haskell', label: 'Haskell' },
      { value: 'clojure', label: 'Clojure' }
    ];

    options.forEach(opt => {
      const option = document.createElement('option');
      option.value = opt.value;
      option.textContent = opt.label;
      select.appendChild(option);
    });

    this.appendChild(select);

    // Initialize Choices.js
    this.choices = new Choices(select, {
      removeItemButton: this.getAttribute('remove-button') === 'true',
      placeholder: true,
      placeholderValue: this.getAttribute('placeholder') || 'Select options',
      searchEnabled: true,
      searchPlaceholderValue: 'Search...'
    });

    // Listen for changes
    select.addEventListener('change', (e) => {
      const selectedValues = Array.from(select.selectedOptions).map(opt => opt.value);

      this.dispatchEvent(new CustomEvent('choicesChange', {
        detail: {
          value: selectedValues
        },
        bubbles: true,
        composed: true
      }));
    });
  }

  disconnectedCallback() {
    if (this.choices) {
      this.choices.destroy();
    }
  }
}

// Register the custom element
customElements.define('choices-multi-select', ChoicesMultiSelect);
