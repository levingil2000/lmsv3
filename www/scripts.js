// scripts.js

$(document).ready(function() {
  // Initialize AOS
  AOS.init({
    duration: 800,
    easing: 'ease-out-cubic',
    once: true,
    offset: 50
  });

  // Scroll progress indicator
  $(window).scroll(function() {
    var scroll = $(window).scrollTop();
    var height = $(document).height() - $(window).height();
    var scrolled = (scroll / height) * 100;
    $('.scroll-indicator').css('width', scrolled + '%');
  });

  // Add scroll indicator to body
  $('body').prepend('<div class="scroll-indicator"></div>');

  // Parallax effect for background
  $(window).scroll(function() {
    var scrolled = $(this).scrollTop();
    var parallax = $('.parallax-bg');
    var speed = 0.5;
    parallax.css('transform', 'translateY(' + (scrolled * speed) + 'px)');
  });

  // Number counting animation
  function animateValue(id, start, end, duration) {
    var range = end - start;
    var current = start;
    var increment = end > start ? 1 : -1;
    var stepTime = Math.abs(Math.floor(duration / range));
    var obj = $(id);
    var timer = setInterval(function() {
      current += increment;
      obj.text(current);
      if (current == end) {
        clearInterval(timer);
      }
    }, stepTime);
  }

  // Trigger counting animation when value boxes come into view
  $('.value-box').each(function() {
    var $this = $(this);
    $(window).scroll(function() {
      var elementTop = $this.offset().top;
      var elementBottom = elementTop + $this.outerHeight();
      var viewportTop = $(window).scrollTop();
      var viewportBottom = viewportTop + $(window).height();

      if (elementBottom > viewportTop && elementTop < viewportBottom) {
        if (!$this.hasClass('animated')) {
          $this.addClass('animated');
          var targetValue = parseInt($this.find('h3').text()) || 0;
          if (targetValue > 0) {
            $this.find('h3').text('0');
            animateValue($this.find('h3'), 0, targetValue, 2000);
          }
        }
      }
    });
  });

  // Staggered fade-in for chart containers
  setTimeout(function() {
    $('.chart-container').each(function(index) {
      $(this).delay(index * 200).animate({
        opacity: 1,
        transform: 'translateY(0)'
      }, 600);
    });
  }, 1000);
});

// Initialize AOS on first load
$(document).ready(function () {
    AOS.init({
        once: false, // animations repeat on scroll
        duration: 800 // adjust speed
    });
});

// Re-run AOS after Shiny renders new content
$(document).on('shiny:value', function() {
    AOS.refresh();
});