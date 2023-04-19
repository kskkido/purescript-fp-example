exports.fromStringImpl = function (just) {
  return function (nothing) {
    return function (htmlStr) {
      return function () {
        const template = document.createElement('template');
        template.innerHTML = htmlStr.trim();
        const node = template.content.firstChild;
        if (node instanceof HTMLElement) {
          return just(node);
        } else {
          return nothing;
        }
      };
    };
  };
};

exports.setStylesImpl = function (obj) {
  return function (htmlElement) {
    return function () {
      Object.entries(obj).forEach((entry) => {
        htmlElement.style[entry[0]] = entry[1];
      });
    };
  };
};
