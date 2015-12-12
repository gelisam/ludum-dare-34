function visual_guide() {
    var game_height = 400;
    var game_width = 400;
    var scene = sjs.Scene({w:game_width, h:game_height});

    scene.loadImages(['img/bird.png'], function() {

        setTimeout(function() {
            var bird = scene.Sprite('img/bird.png');
            bird.position(200, 200);
            bird.move(-10, -10);
            bird.rotate(Math.PI / 3.0);
            bird.scale(bird.xscale + 0.5, bird.yscale + 0.5);
            bird.setOpacity(bird.opacity - 0.2);
            bird.update();
        }, 100);

    });
}
