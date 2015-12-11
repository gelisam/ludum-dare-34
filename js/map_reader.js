function map_reader() {
    sjs.debug = true;

    var w = Math.min(window.innerWidth, 1024);
    var h = Math.min(window.innerHeight, 800);

    var scene = sjs.Scene({w:w, h:h, autoPause:false});

    scene.loadImages(['ground.png', 'tiles.png'], function() {
        scene.main = main;

        sjs.map.loadMap('map.json', scene);

        var surface;

        function main() {
            if(surface)
                surface.remove();

            surface = sjs.ScrollingSurface(scene, scene.w, scene.h, function(layer, _x, _y) {
                sjs.map.paintOn(layer, _x, _y);
            });

            surface.update();
        }
    });
}
