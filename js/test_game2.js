function test_game2() {


    var game_height = 480;
    var game_width = 640;
    var scene = sjs.Scene({w:game_width, h:game_height});
    var gravity = 0.5;

    scene.loadImages(['img/character.png', 'img/crate.png'], function() {

    var back = scene.Layer('backround');
    var front = scene.Layer('front')
    var score = scene.Sprite(false, front);
    score.size(200, 100);
    score.position(20, 20);
    score.update();
    score.dom.id = 'score';

    var bottom = scene.Sprite('img/crate.png', {layer:back, w:game_width, h:64, x:0, y:game_height-64});
    bottom.update();
    var elements = sjs.SpriteList();
    elements.add(bottom);

    var player = scene.Sprite('img/character.png', front);
    player.position(40, 200);
    player.size(28, 52);
    player.scale(-1, 1);

    var input  = scene.Input();

    var cycle = scene.Cycle([[3, 3, 5],
                               [33, 3, 5],
                               [63, 3, 5],
                               [93, 3, 5],
                               [123, 3, 5],
                               [153, 3, 5],
                               [183, 3, 5]]);
    cycle.addSprite(player);

    var virtual_player_x = player.x;
    var player_xv = 2.5;
    var score_count = 0;

    function paint() {

        player.yv += gravity;
        player.applyXVelocity();
        if(player.collidesWithArray(elements)) {
            ticker.pause();
            alert("Game over!");
            return;
        }

        player.applyYVelocity();
        if(player.collidesWithArray(elements)) {
            player.reverseYVelocity();
            player.yv = 0;
            if(input.mousedown || input.keydown) {
                player.yv = -10;
            }
        } else {
            if(input.mousedown || input.keydown) {
                player.yv -= 0.2;
            }
        }

        player.update();

        var el;
        var need_to_create_plateform = true;
        while(el = elements.iterate()) {
            el.xv = -player_xv;
            el.applyVelocity();
            el.update();

            if(el.isPointIn(game_width, game_height-20)) {
                need_to_create_plateform = false;
            }

            if(el.x + el.w < 0) {
                elements.remove(el)
            }
        }

        if(need_to_create_plateform && Math.random() < 0.1) {
            var height = 32 + (Math.random() * 96);
            var width = 64 + (Math.random() * 128);
            var bottom = scene.Sprite('img/crate.png', {layer:back, w:width, h:height, x:game_width, y:game_height-height});
            bottom.update();
            elements.add(bottom);
        }

        cycle.next(ticker.lastTicksElapsed);

        player_xv += 0.002;
        score_count += 0.08;
        score.dom.innerHTML = 'Score '+Math.round(score_count);

        if(player.y > game_height) {
            ticker.pause();
            alert("Game over");
            return;
        }
    };

    var ticker = scene.Ticker(25, paint);
    ticker.run();

    });

};
