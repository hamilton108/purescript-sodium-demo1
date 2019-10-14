"use strict";

exports.mouse_event = function (evt) {
    return function (draggable) {
        return function () {
            console.log(evt);
            return { y: evt.offsetY, draggable: draggable };
        };
    };
};
