function visual_guide() {
    var game_height = 400;
    var game_width = 400;
    var scene = sjs.Scene({w:game_width, h:game_height});
    var exec_list = document.getElementById('exec_list');
    var bird = scene.Sprite('img/bird.png');

    scene.loadImages(['img/bird.png'], function() {

        var function_array = [
            {'name': 'Initialization',
                'execute':function () {
bird = scene.Sprite('img/bird.png');
bird.update();
            }},
            {'name': 'Set position',
                'execute':function () {
bird.position(200, 200);
bird.update();
            }},
            {'name': 'Move',
                'execute':function () {
bird.move(-10, -10);
bird.update();
            }},
            {'name': 'Rotate',
                'execute':function () {
bird.rotate(Math.PI / 3.0);
bird.update();
            }},
            {'name': 'Scale',
                'execute':function () {
bird.scale(bird.xscale + 0.5, bird.yscale + 0.5);
bird.update();
            }},
            {'name': 'Set opacity',
                'execute':function () {
bird.setOpacity(bird.opacity - 0.2);
bird.update();
            }},
        ]

        var list_str = "";
        for(var i=0; i<function_array.length; i++) {
            var el = function_array[i];
            var code = String(el['execute']).split('{')[1].split('}')[0].replace(/^\s+|\s+$/g, '');

            list_str = list_str + '<li><h2>'+ el.name
              +'</h2><pre>'+
              code
              +'</pre><button id="exec_'+

              i+'">Execute</button></li>';

        }

        exec_list.innerHTML = list_str;
        exec_list.onclick = function(e) {
            var index = parseInt(e.target.id.split('_')[1]);
            if(function_array[index])
                function_array[index].execute();
        }

    });
}
