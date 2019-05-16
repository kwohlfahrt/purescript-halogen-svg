// module Svg.Util

exports._beginElements = function (cssSelectorStr) {
  return function (onError, onSuccess) {
    console.log('beginElementsAff: cssSelectorStr =', cssSelectorStr);
    const elems = document.querySelectorAll(cssSelectorStr);
    console.log('beginElementsAff: elems =', elems);
    const numElems = elems.length || -1;
    console.log('beginElementsAff: number of elements found: ', numElems);

    elems.forEach(function (elem) {
      elem.beginElement();
    });

    // TODO onError case?
    onSuccess(numElems);

    return function (cancelError, cancelerError, cancelerSuccess) {
      cancelerSuccess(); // no cleanup to do
    };
  };
}

exports.domToSvgCoordinates = function (svg) {
  return function (point) {
    const svgPoint = svg.createSVGPoint();

    svgPoint.x = point.x;
    svgPoint.y = point.y;

    const svgCoordPoint = svgPoint.matrixTransform(svg.getScreenCTM().inverse());

    return {
      x: svgCoordPoint.x,
      y: svgCoordPoint.y,
      z: 0
    };
  };
}
