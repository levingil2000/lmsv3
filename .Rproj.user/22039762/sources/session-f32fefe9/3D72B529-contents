// This script should be placed in a file named 'custom-animations.js' inside the 'www' folder.

// Wait for the DOM to be fully loaded before running the scripts
document.addEventListener("DOMContentLoaded", function() {

  // --- 1. Parallax Effect Handler ---
  const parallaxBg = document.getElementById('parallax-background');
  if (parallaxBg) {
    window.addEventListener('scroll', function() {
      const offset = window.pageYOffset;
      // You can adjust the 0.4 value to make the parallax effect more or less subtle
      parallaxBg.style.transform = 'translateY(' + offset * 0.4 + 'px)';
    });
  }

  // --- 2. Fade-in on Scroll Handler ---
  // Select all elements that should fade in
  const fadeInElements = document.querySelectorAll('.fade-in-element');

  // Check if there are any elements to observe
  if (fadeInElements.length > 0) {
    // Create a new Intersection Observer
    const observer = new IntersectionObserver((entries, observer) => {
      entries.forEach(entry => {
        // When the element comes into the viewport
        if (entry.isIntersecting) {
          // Add the 'is-visible' class to trigger the CSS animation
          entry.target.classList.add('is-visible');
          // Stop observing the element once it has become visible to save resources
          observer.unobserve(entry.target);
        }
      });
    }, {
      threshold: 0.1 // Trigger the animation when 10% of the element is visible
    });

    // Attach the observer to each of the fade-in elements
    fadeInElements.forEach(element => {
      observer.observe(element);
    });
  }
});
