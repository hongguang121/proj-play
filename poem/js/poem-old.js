/**
 * Created by Administrator on 2017/3/2 0002.
 */

    //高度控制，页面在不同设备和下浏览器上的显示，允许删除，查找商品

var receive_json = {};
var evt_lst = {evt: []}; //添加内容顺序表
var evt_cnt = 0;
var remb_text = {text: {title: '', author: '', summary: '', contents: []}}; //文本缓存 {text_id: 'text_0', content: 'test0'}, {text_id: 'text_1', content: 'test1'}
var remb_pic = {images: []}; //图片缓存
var remb_good = {goods: []}; //商品缓存
var remb_cnt = 0;
var sum_high = 0;
var add = 0; //限制‘添加新的图片’按钮点击次数
//good-cache 商品缓存 {goods: [{good_id: '', good_name: '', good_link: '', good_img: ''}]} 直接从服务器获取

//模拟商品列表，最后通过ajax在页面加载时从服务器读取
var goods = {good: [{good_id: "1", good_name: "洗发水", good_url: "a", good_price: 65, good_pic: "img/pic/background.jpg"},
    {good_id: "2", good_name: "沐浴露", good_url: "b", good_price: 70, good_pic: "img/pic/index0.jpg"},
    {good_id: "3", good_name: "护发素", good_url: "c", good_price: 55, good_pic: "img/pic/index1.jpg"}]};

var cnt_pic = 0; //用来生成id
var curr_pic_cnt = 0; //限制图片上传数量，不能超过5

var extinct = "gs_good_0"; //熄灭修改图标

$(document).ready(function() {

    //屏幕大小适应
    var curr_screen_width = $(window).width();
    var main_view_width = 1280;
    var left_shifting = (curr_screen_width - main_view_width) / 2;

    var left_change = function(){
        $(".head").css("left", left_shifting + "px");
        $(".left-panel").css("left", (left_shifting + 1) + "px");
        $(".middle-panel").css("left", left_shifting + "px");
        $(".right-panel").css("left", left_shifting + "px");
    };

    left_change();

    $(window).resize(function(){
        curr_screen_width = $(window).width();
        left_shifting = (curr_screen_width - main_view_width) / 2;
        left_change();

        if (curr_screen_width < 1280){

        }
    });

    //高度调整
    var change_high = function(){
        var hg = $(".middle-panel").height();
        console.log(hg);
        $(".left-panel").css("height", hg + 2);
        $(".right-panel").css("height", hg + 2);
        $(".middle-panel").height();
    };

    change_high();


    $("#title").keydown(function(evt){
        switch (evt.keyCode){
            case 13: $("#auth").select();
        }
    });

    var good_list_id = "";
    //将商品读入左侧商品列表 good_list_*：表示左侧商品列表排列顺序
    var to_list = function() {
        for (var id = 0; id < goods.good.length; id++) {
            good_list_id = "good_list_id_" + id;
            $(".goods-list ul").append("<li class='list-group-item' id=" + good_list_id + ">" + goods.good[id].good_name + "</li>");
        }
    };

    to_list();

    var curr_shop = {
        "position": "relative",
        "width": "150px",
        "height": "150px",
        "margin-top": "25px",
        "margin-left": "25px",
        "margin-right": "15px",
        "margin-bottom": "25px",
        "overflow": "hidden",
        "opacity": "0.8",
        "background": "url(img/img-2.png) center no-repeat",
        "background-color": "#9c9c9c",
        "float": "left"
    };

    var curr_img_shop = {
        "position": "relative",
        "height": "150px",
        "overflow": "hidden",
        "opacity": "0",
        "cursor": "pointer"
    };

    //将选中商品添加到商品展示框
    var gid = "";
    var sid = 0; //商店名
    $(".goods-list ul li").click(function(){
        good_list_id = $(this).attr("id");
        //alert(good_list_id);
        gid = good_list_id.substring(13);
        evt_lst.evt[evt_cnt] = "good_" + gid; //添加商品id进入队列
        evt_cnt = evt_cnt + 1;
        remb_good.goods[remb_cnt] = goods.good[gid];
        remb_cnt = remb_cnt + 1;
        $(".link").append(show_shop(sid));//画出商店
        show_good(gid);//展示商品
    });

    //gid:商品id，也是remb_good.goods[id]
    var show_shop = function(sid){
        var title = 'title_' + sid;
        var link = 'link_' + sid;
        var price = 'price_' + sid;
        var up = 'upload_shop_' + sid;

        return "<div class='shop'>" +
            "<div class='intro-img'>" +
            "<div class=" + up + ">" +
            "</div>" +
            "</div>" +
            "<div class='intro-info'>" +
            "<div class='shop-info'>" +
            "<input name='title' type='text' id=" + title + " size='30' class='init-title' maxlength='100' readonly autocomplete='off' placeholder='商品名称'; style='border:none;height:40px;background-color:whitesmoke;border-radius:5px;font-size:large;color:dimgrey;' />" +
            "<input name='link' type='text' id=" + link + " size='30' class='init-title' maxlength='100' readonly autocomplete='off' placeholder='商品链接' style='border:none;height:40px;background-color:whitesmoke;border-radius:5px;font-size:large;margin-top:10px;color:dimgrey;' />" +
            "<input name='price' type='text' id=" + price + " size='30' class='init-title' maxlength='100' readonly autocomplete='off' placeholder='商品价格' style='border:none;height:40px;background-color:whitesmoke;border-radius:5px;font-size:large;margin-top:10px;color:dimgrey;' />" +
            "</div></div></div>"
    };

    var show_good = function(gid){
        $("#title_" + sid).val(goods.good[gid].good_name);
        $("#link_" + sid).val(goods.good[gid].good_url);
        $("#price_" + sid).val(goods.good[gid].good_price);
        $(".upload_shop_" + sid).css(curr_shop);
        $(".upload_shop_" + sid).css({
            "background": "url(" + goods.good[gid].good_pic + ") center",
        });
        sid = sid + 1; //新的商店
        add = 0;
        sum_high = sum_high + $(".new_shop").height();
        change_high();
    };

    $(".edit-pic").click(function(){
        var curr_good_select_id = $(this).attr("id");
        //$("#" + extinct).css("color", "grey");
        $("#" + curr_good_select_id).css("color", "red");
        extinct = curr_good_select_id;
        shop_id = curr_good_select_id.substring(3);
        //alert(curr_good_select_id);
    });

    ////////////////////////////////////////////////////////////////////////////////

        var img_upload_css = {
            "position": "relative",
            "width": "150px",
            "height": "150px",
            "border": "dashed 1px #c4c4c4",
            "margin-top": "20px",
            "margin-left": "25px",
            "margin-right": "15px",
            "margin-bottom": "25px",
            "overflow": "hidden",
            "opacity": "0.5",
            "background": "url(img/img-1.png) center no-repeat",
            "background-color": "#9c9c9c",
            "float": "left"
        };

        var curr_image_css = {
            "position": "relative",
            "height": "150px",
            "overflow": "hidden",
            "opacity": "0",
            "cursor": "pointer"
        };

        var curr_pic_css = {
            "position": "relative",
            "width": "150px",
            "height": "150px",
            "border": "dashed 1px #c4c4c4",
            "margin": "20px 15px 25px 25px",
            "overflow": "hidden",
            "float": "left",
            "z-index": "1"
        };

        var curr_close_css = {
            "position": "relative",
            "width": "30px",
            "height": "30px",
            "border": "solid 1px #c4c4c4",
            "border-radius": "20px",
            "background-color": "#ffffff",
            "float": "right",
            "opacity": "0.8",
            "z-index": "2",
            "text-align": "center",
            "font-size": "small",
            "padding-top": "6px",
            "padding-left": "1px",
            "color": "red",
            "cursor": "pointer"
        };

        $(".image-upload").change(function () {
            if (curr_pic_cnt < 5){
                var class_id = "show_upload_0_img_id_" + cnt_pic;
                var close_id = "close_" + cnt_pic;
                remb_pic.images[cnt_pic] = {img_id: class_id, img_url: "img/pic/" + $(".image").val()};
                //alert(remb_pic.images[cnt_pic].img_id + ": " + remb_pic.images[cnt_pic].img_url);

            $(".show_upload").append("<div class=" + class_id + ">" +
                "<div class=" + close_id + ">" +
                "<span class=\"glyphicon glyphicon-remove\" aria-hidden=\"true\"></span>" +
                "</div>" +
                    //"<img src=" + remb_pic[cnt_pic] + " width='150'/>" +
                "</div>");

            $("." + class_id).css(curr_pic_css);
            $("." + close_id).css(curr_close_css);

            $("." + class_id).css({
                "background": "url(" + remb_pic.images[cnt_pic].img_url + ") no-repeat center",
                "background-size:": "cover"
            });

            $("." + close_id).click(function () {
                $("." + class_id).remove();
                var index = parseInt(class_id.substring(21));
                delete remb_pic.images[index];
                //remb_pic = useful(del(remb_pic, index));
                curr_pic_cnt = curr_pic_cnt - 1;
            });

            cnt_pic = cnt_pic + 1;
            curr_pic_cnt = curr_pic_cnt + 1;
            change_high();

                $.post("/common/resource/image/upload",
                    function(img_url){
                        console.log(img_url);
                    });
            }
        });

    ///////////////////////////////////////////////////////////////////////////////////////

        var shop = [];
        var shop_cnt = 0;

        //暂时关闭此功能，不允许上传商品图片
        //$(".upload_shop_0").change(function () {
        //    shop[shop_cnt] = "img/pic/" + $(".image-shop").val();
        //    var shop_id = "shop_id_" + shop_cnt;
        //    $(".upload_shop_0").css({
        //        "background": "url(" + shop[shop_cnt] + ") no-repeat center",
        //        "background-size:": "cover"
        //    });
        //});

    ///////////////////////////////////////////////////////////////////////////////////////////////

    $(document).ready(function(){

        /**********************************************************************************************/

        var show_upload = ['show_upload_0_']; //添加显示图片的区域
        var show_cnt = 1;
        var img_id = ['image'];
        var img_cnt = 1;
        var img_upload_id = ['image-upload'];
        var img_up_cnt = 1;

        $("#add_img").click(function(){
            if (add == 0) {
                show_upload[show_cnt] = "show_upload_" + show_cnt; //show_cnt目前为1位，需要处理1位以上
                img_id[img_cnt] = "image_" + img_cnt;
                img_upload_id = "image-upload-" + img_up_cnt;
                evt_lst.evt[evt_cnt] = show_upload[show_cnt];
                evt_cnt = evt_cnt + 1;
                var img_upload_name = "." + img_upload_id;
                var curr_pic_cnt = 0; ////限制图片上传数量，不能超过5

                $(".link").append(
                    "<div class=" + show_upload[show_cnt] + "></div>" +
                    "<div class=" + img_upload_id + ">" +
                    "<input name=\"upload\" type=\"file\" multiple class=" + img_id[img_cnt] + " /></div>");

                var img_name = "." + img_id[img_cnt];
                var show_name = "." + show_upload[show_cnt];
                var show_path = show_upload[show_cnt];
                //alert(show_path);

                $(img_upload_name).css(img_upload_css);
                $(img_name).css(curr_image_css);

                $(img_upload_name).change(function () {
                    if (curr_pic_cnt < 5) {
                        var class_id = show_path + "_img_id_" + cnt_pic;
                        var close_id = "close_" + cnt_pic;
                        //remb_pic[cnt_pic] = "img/pic/" + $(img_name).val();
                        remb_pic.images[cnt_pic] = {img_id: class_id, img_url: "img/pic/" + $(img_name).val()};
                        //alert(remb_pic.images[cnt_pic].img_id + ": " + remb_pic.images[cnt_pic].img_url);
                        $(show_name).append("<div class=" + class_id + ">" +
                            "<div class=" + close_id + ">" +
                            "<span class=\"glyphicon glyphicon-remove\" aria-hidden=\"true\"></span>" +
                            "</div>" +
                            "</div>");
                        var class_name = "." + class_id;
                        var close_name = "." + close_id;
                        $(class_name).css(curr_pic_css);
                        $(close_name).css(curr_close_css);
                        $(class_name).css({
                            "background": "url(" + remb_pic.images[cnt_pic].img_url + ") no-repeat center",
                            "background-size:": "cover"
                        });
                        $(close_name).click(function () {
                            $(class_name).remove();
                            var index = parseInt(class_id.substring(19)); //未来用变量
                            delete remb_pic.images[index];
                            //remb_pic = del(remb_pic, index);
                            curr_pic_cnt = curr_pic_cnt - 1;
                        });
                        cnt_pic = cnt_pic + 1;
                        curr_pic_cnt = curr_pic_cnt + 1;
                        change_high();
                    }
                });
                change_high();
                img_cnt = img_cnt + 1;
                img_up_cnt = img_up_cnt + 1;
                show_cnt = show_cnt + 1;
                add = 1; //不允许在本段落操作中再次点击添加图片按钮
            }
        });

        /**********************************************************************************************/

        var art_id = ['text'];
        var art_cnt = 1;
        $("#add_art").click(function(){
            art_id[art_cnt] = "text_" + art_cnt;
            evt_lst.evt[evt_cnt] = art_id[art_cnt];
            evt_cnt = evt_cnt + 1;
            remb_text.text.contents[art_cnt] = {text_id: art_id[art_cnt], content: ""};
            //alert(remb_text.text.contents[art_cnt].text_id + ": " + remb_text.text.contents[art_cnt].content);
            //alert(remb_text.text.contents[art_cnt].text_id + ": " + remb_text.text.contents[art_cnt].content);
            //alert(remb_text.text.contents);
            $(".link").append(
                "<textarea name=\"content\" type=\"text\" class=\"init-text\" id=" + art_id[art_cnt] + " size=\"30\" maxlength=\"5000\"" +
                "style=\"resize:none;overflow-x:hidden;overflow-y:visible;font-size:medium;width:100%;height:300px;margin-top:25px;margin-bottom:25px;color:#363636\" ></textarea>");
            art_cnt = art_cnt + 1;
            change_high();
            add = 0;
        });

        /**********************************************************************************************/

/*      var good_id = ['good_0']; //展示商店的id
        var good_cnt = 1;
        var upload_shop_id = ['upload_shop_0'];
        var shop_cnt = 1;
        var image_shop_id = ['image-shop'];
        var image_shop_cnt = 1;

        $("#add_good").click(function(){
            good_id[good_cnt] = "good_" + good_cnt;
            //evt_lst.evt[evt_cnt] = good_id[good_cnt]; 不应在此处添加gid
            upload_shop_id[shop_cnt] = "upload_shop_" + shop_cnt;
            image_shop_id[image_shop_cnt] = "image-shop-" + image_shop_cnt;

            var curr_shop = {
                "position": "relative",
                "width": "150px",
                "height": "150px",
                "margin-top": "25px",
                "margin-left": "25px",
                "margin-right": "15px",
                "margin-bottom": "25px",
                "overflow": "hidden",
                "opacity": "0.8",
                "background": "url(img/img-2.png) center no-repeat",
                "background-color": "#9c9c9c",
                "float": "left"
            };

            var curr_img_shop = {
                "position": "relative",
                "height": "150px",
                "overflow": "hidden",
                "opacity": "0",
                "cursor": "pointer"
            };

            var good_select_id = "gs_" + good_id[good_cnt];
            var title_id = "shop_title_" + good_cnt;
            var link_id = "shop_link_" + good_cnt;
            var price_id = "shop_price_" + good_cnt;


            $(".link").append(
                "<div class=\"shop\" id=" + good_id[good_cnt] + ">" +
                "<div class=\"intro-img\">" +
                "<div class=" + upload_shop_id[shop_cnt] + ">" + //"<input name=\"upload-shop\" type=\"file\" multiple class=" + image_shop_id[image_shop_cnt] + " />
                "</div></div>" +
                "<div class=\"intro-info\">" +
                "<div class=\"shop-info\">" +
                "<input name=\"title\" type=\"text\" id=" + title_id + " placeholder=\"商品名称\" size=\"30\" class=\"init-title\" maxlength=\"100\" readonly autocomplete=\"off\"" +
                "style=\"border:none;height:40px;background-color:whitesmoke;border-radius:5px;font-size:large;color:dimgrey;\" />" +
                "<input name=\"link\" type=\"text\" id=" + link_id + " placeholder=\"商品链接\" size=\"30\" class=\"init-title\" maxlength=\"100\" readonly autocomplete=\"off\"" +
                "style=\"border:none;height:40px;background-color:whitesmoke;border-radius:5px;font-size:large;margin-top:10px;color:dimgrey;\" />" +
                "<input name=\"price\" type=\"text\" id=" + price_id + " placeholder=\"商品价格\" size=\"30\" class=\"init-title\" maxlength=\"100\" readonly autocomplete=\"off\"" +
                "style=\"border:none;height:40px;background-color:whitesmoke;border-radius:5px;font-size:large;margin-top:10px;color:dimgrey;\" /></div></div>" +
                "<div class=\"good-select\">" +
                "<span class=\"glyphicon glyphicon-pencil edit-pic\" id=" + good_select_id + " aria-hidden=\"true\"></span>" +
                "</div>" +
                "</div>");

            $("." + upload_shop_id[shop_cnt]).css(curr_shop);
            $("." + image_shop_id[image_shop_cnt]).css(curr_img_shop);

            var img_name = "." + image_shop_id[image_shop_cnt];  //注意这里的名字
            var up_name = "." + upload_shop_id[shop_cnt];
            //alert(up_name);
            $(up_name).change(function () {
                var shop_img_path = $(img_name).val();
                shop[shop_cnt] = "img/pic/" + shop_img_path;
                //alert(shop[shop_cnt]);
                $(up_name).css(
                    "background", "url(" + shop[shop_cnt] + ") no-repeat center"
                );
            });

            change_link_high(good_id[good_cnt]);
            image_shop_cnt = image_shop_cnt + 1;
            shop_cnt = shop_cnt + 1;
            good_cnt = good_cnt + 1;
            add = 0;

            //$(".goods-list ul li").mouseover(function(){
            //    var curr_good_id = $(this).attr("id");
            //    good_id = curr_good_id;
            //    $("#" + curr_good_id).click(function(){
            //        show_good(curr_good_id, shop_id);
            //    });
            //});

            $(".edit-pic").click(function(){
                var curr_good_select_id = $(this).attr("id");
                $("#" + extinct).css("color", "grey");
                $("#" + curr_good_select_id).css("color", "red");
                extinct = curr_good_select_id;
                shop_id = curr_good_select_id.substring(3);
                //alert(shop_id);
            });
        });*/
    });

    ////////////////////////////////////////////////////////////////////////////////

    var words = 0;
    $("#auth").keyup(function() {
        words = $('#auth').val();
        $("#count-auth").html("<p>" + words.length + "/25</p>")
    });

    var cont_words = 0;
    $("#text_0").keyup(function() {
        cont_words = $("#text_0").val();
        $("#count-cont").html("<p id=\"cnt\">" + cont_words.length + "/5000</p>")
    });

    ////////////////////////////////////////////////////////////////////////////////

    $("#final").click(function(){
        var text_val = $("#text_0").val();
        //console.log(text_val);
        remb_text.text.title = $("#title").val();
        remb_text.text.author = $("#auth").val();
        remb_text.text.summary = $("#summary").val();
        //console.log(remb_text.text.summary = $("#summary").val());
        remb_text.text.contents[0] = {text_id: 'text_0', content: text_val};

        //alert(remb_text.text.title + ":" + remb_text.text.author + ":" + remb_text.text.summary);

        //remb_text = {text: {title: '', author: '', summary: '', contents: [{text_id: '', content: ''}]}}

        for(var i = 0; i < remb_text.text.contents.length; i++){
            remb_text.text.contents[i].content = $("#" + remb_text.text.contents[i].text_id).val();
            //alert(remb_text.text.contents[i].text_id + ": " + remb_text.text.contents[i].content);
        }

        //for (var i = 0; i < evt_lst.evt.length; i++){
        //    alert(evt_lst.evt[i]);
        //}

        //console.log(remb_text.text);
        //console.log(remb_pic.images);
        //console.log(remb_good.goods);
        //console.log(evt_lst);

        //evt_lst:预览或取回数据的顺序链
        var send_json = {blog: [evt_lst, remb_text.text, remb_pic.images, remb_good.goods]};
        //alert(send_json);
        console.log(JSON.stringify(send_json));

        $.post("/", send_json, function(data){
            receive_json = data;
        });

        //自动添加summary
        document.getElementById("summary").innerText = text_val.substring(0, 200);

        if(remb_text.text.title == ""){
            alert('请输入文章标题');
        }

        if(text_val == ""){
            alert('请输入文章');
        }

        if(remb_text.text.summary == ""){
            remb_text.text.summary = text_val.substring(0, 200);
        }

/*        if(remb_pic.images[0] == undefined){
            alert('请至少上传一张图片');
        }

        if(remb_good.goods[0] == undefined){
            alert('请插入一条商品信息');
        }*/

        //autocomplete="off"

    });

    //text:
    var show_title = function(){
        return "<div class='new_title'><h2>" + remb_text.text.title + "</h2>" +
                                      "<h4>作者: " + remb_text.text.author + "</h4>" +
                                      "<p class='smry'>" + remb_text.text.summary + "</p></div>"
    };

    var show_text = function(tid){
        //return "<div class='txt'><p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"  + remb_text.text.contents[tid].content + "</p></div>";
        return "<div class='txt'><p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"  + remb_text.text.contents[tid].content + "</p></div>";
    };

    var show_img = function(pid){
        return "<div class='imgs'><img src=" + remb_pic.images[pid].img_url + " width='750' id='reset' /></div>"
    };

    var show_shop = function(gid){
        var title = 'title_' + gid;
        var link = 'link_' + gid;
        var price = 'price_' + gid;
        var up = 'upload_shop_' + gid;

        return "<div class='new_shop'>" +
            "<div class='intro-img'>" +
            "<div class=" + up + ">" +
            "</div>" +
            "</div>" +
            "<div class='intro-info'>" +
            "<div class='shop-info'>" +
            "<input name='title' type='text' id=" + title + " size='30' class='init-title' maxlength='100' readonly autocomplete='off' placeholder='商品名称'; style='border:none;height:40px;background-color:whitesmoke;border-radius:5px;font-size:large;color:dimgrey;' />" +
            "<input name='link' type='text' id=" + link + " size='30' class='init-title' maxlength='100' readonly autocomplete='off' placeholder='商品链接' style='border:none;height:40px;background-color:whitesmoke;border-radius:5px;font-size:large;margin-top:10px;color:dimgrey;' />" +
            "<input name='price' type='text' id=" + price + " size='30' class='init-title' maxlength='100' readonly autocomplete='off' placeholder='商品价格' style='border:none;height:40px;background-color:whitesmoke;border-radius:5px;font-size:large;margin-top:10px;color:dimgrey;' />" +
            "</div></div></div>"
    };

    //预览
    var pre_lock = 1; //防止多次点击生成新节点
    $(".t").click(function(){
        if (pre_lock == 1) {
            $(".middle-panel").append("<div class='preview'></div>");
            var text_val = $("#text_0").val();
            remb_text.text.contents[0] = {text_id: 'text_0', content: text_val};
            document.getElementById("summary").innerText = text_val.substring(0, 200);
            var text_val = $("#text_0").val();
            remb_text.text.title = $("#title").val();
            remb_text.text.author = $("#auth").val();
            remb_text.text.summary = $("#summary").val();
            remb_text.text.contents[0] = {text_id: 'text_0', content: text_val};

            for (var i = 0; i < remb_text.text.contents.length; i++) {
                remb_text.text.contents[i].content = $("#" + remb_text.text.contents[i].text_id).val();
            }

            make_pre();
            $(".title").hide();
            $(".contents").hide();
            $(".preview").show();
            $(".p").css("background-color", "white");
            change_high();
            //console.log("preview:" + $(".middle-panel").height());
            pre_lock = 0;
        }
    });

    //编辑
    $(".p").css("background-color", "#eee");
    $(".p").click(function(){
        $(".p").css("background-color", "#eee");
        $(".title").show();
        $(".contents").show();
        $(".preview").remove();
        change_high();
        pre_lock = 1;
    });

    //生成预览
    var make_pre = function(){
        //必须显示
        $(".preview").append(
            show_title() +
            show_text(0)
        );
        //按组显示上传照片
        for (var i = 0; i < remb_pic.images.length; i++) {
            if (remb_pic.images[i].img_id.substring(12, 13) == 0) {
                $(".preview").append("<div class='imgs'><img src=" + remb_pic.images[i].img_url + " width='670' /></div>");
                /*$(".preview").append("<div class='imgs'></div>");
                $(".imgs").css({
                    "background": "url(" + remb_pic.images[i].img_url + ") center"
                });*/
            }
        }
        var evt = evt_lst.evt;
        //alert(evt);
        //console.log(evt == "");

        if(evt != "") {
            for (var ev in evt) {
                if (evt_lst.evt[ev].substring(0, 4) == "text") { //文本
                    var tid = parseInt(evt_lst.evt[ev].substring(5));
                    //console.log(tid);
                    remb_text.text.contents[tid].content = $("#" + remb_text.text.contents[tid].text_id).val();
                    $(".preview").append(show_text(parseInt(evt_lst.evt[ev].substring(5))));
                    //console.log(show_text(parseInt(evt_lst.evt[ev].substring(5))));
                } else
                if (evt_lst.evt[ev].substring(0, 4) == "show") { //图片
                    var flag = evt_lst.evt[ev].substring(12); //获取show_upload_*
                    console.log(flag);
                    //remb_pic.images[cnt_pic] = {img_id: class_id, img_url: "img/pic/" + $(img_name).val()};
                    //console.log(remb_pic.images[0].img_id);
                    //console.log(remb_pic.images[1].img_id);
                    for (var i = 0; i < remb_pic.images.length; i++) {
                        //console.log(remb_pic.images[i].img_id.substring(12, 13));
                        if (remb_pic.images[i].img_id.substring(12, 13) == flag) {
                            //console.log(remb_pic.images[i].img_id);
                            $(".preview").append("<div class='imgs'><img src=" + remb_pic.images[i].img_url + " width='670' /></div>");
                        }
                    }
                } else
                if (evt_lst.evt[ev].substring(0, 4) == "good"){ //商品
                    $(".preview").append(show_shop(sid));
                    show_good(evt_lst.evt[ev].substring(5));
                }
            }
        }

        //for(var i = 0; i < remb_text.text.contents.length; i++){
        //    remb_text.text.contents[i].content = $("#" + remb_text.text.contents[i].text_id).val();
        //}
    };

    ///////////////////////////////////////////////////////////////////////////////////////

    //列表无用项清空函数
    var del = function(arr, id){ //id为删除元素下标
        var res = [];
        for(var i = 0; i < arr.length; i++){
           if (i == id) {
               res[i] = 0;
           } else {
               res[i] = arr[i];
           }
        }
        return res;
    };

    var useful = function(arr){
        var res = [];
        var index = 0;
        for(var i = 0; i < arr.length; i++){
            if (arr[i] != 0){
                res[index] = arr[i];
                index = index + 1;
            }
        }
        return res;
    };

    //////////////////////////////////////////////////////////////////////////////////



    //////////////////////////////////////////////////////////////////////////////////

    $(".pb-1").hide();
    $(".pb-2").hide();
    $(".pb-3").hide();
    $(".panel-1").click(function(){
        $(".pb-1").slideToggle('slow');
    });
    $(".panel-2").click(function(){
        $(".pb-2").slideToggle('slow');
    });
    $(".panel-3").click(function(){
        $(".pb-3").slideToggle('slow');
    });

    /*
    function People(name, age){
        this.name = name;
        this.age = age;
        this.hi = function() {
            return "Hi " + this.name;
        }
    }

    var p1 = new People("Yaoer", 12);
    console.log(p1.name);
    console.log(p1.hi());
    */
});
