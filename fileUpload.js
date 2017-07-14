/** * Created by moumou1108 on 2017/4/20.
 */

$(document).ready(function(){
    var loginKey=localStorage.getItem('loginKey');
    var userId=getQueryParam('userId');
    var userType=localStorage.getItem('userType');
    var titleType=getQueryParam('titleType');

    function getQueryParam(name){
        var reg = new RegExp("(^|&)"+ name +"=([^&]*)(&|$)");
        var r = window.location.search.substr(1).match(reg);
        if(r!=null)
            return  unescape(r[2]);
        return null;
    }

    //var url = "http://172.16.9.16:8400/upload";
    var imgUrl = "https://testchangangw.mouchina.com/common/resource/uploadPictures";
    var videoUrl = "https://testchangangw.mouchina.com/common/resource/uploadVideos?loginKey=" + loginKey;
    var url = "http://localhost:8400/upload";
    var submit = "https://testchangangw.mouchina.com/news/add/report";

    var id = 0;
    var evt_id; //图片id
    var close_id; //关闭按钮id
    var count = 0; //上传图片张数
    var showAlive = true;
    var imgInfo = {"photo": []}; //上传图片信息
    var imgData = []; //图片formData编码
    var line = 1; //图片行数
    var videoLink = "";
    var title = ['','儿童拐卖', '疑似疑犯', '烟头不落地',  '肇事违章', '遗失招领','老人走失', '伪基站举报', '其他举报'];
    var location = []; //定位经纬度
    var longitude = 0;
    var latitude = 0;

    $(".hui").click(function(){
        history.back();
    });

    //修改title
    var changeTitle = function(title){
        $(".titleTotal").text(title);
    };

    changeTitle(title[titleType]);

    //删除imgData中的成员
    var delData = function(ls, id){
        var res = [];
        for (var i = 0; i < ls.length; i++){
            if (ls[i].id != id){
                res.push(ls[i]);
            }
        }
        return res;
    };

    //删除imgInfo中的成员
    var delInfo = function(ls, id){
        var res = {"photo": []};
        for (var i = 0; i < ls.photo.length; i++){
            if (ls.photo[i].id != id){
                res.photo.push(ls.photo[i]);
            }
        }
        return res;
    };

    var addPic = function(){
        $("#show").on('tap', function(e){
            e.stopPropagation();
            $(".mask").show();
            $(".approve").stop().animate({
                "bottom":0
            },200);
            $(".approve").find(".call_off").on("tap",function(e){
                e.stopPropagation();
                $(".mask").hide();
                $(".approve").stop().animate({
                    "bottom":"-2rem"
                },200)
            });
        });
    };

    addPic();


    $(".show-video").on('tap', function(e){
        e.stopPropagation();
        $(".mask").show();
        $(".approve-1").stop().animate({
            "bottom":0
        },200);
        $(".approve-1").find(".call_off").on("tap",function(e){
            e.stopPropagation();
            $(".mask").hide();
            $(".approve-1").stop().animate({
                "bottom":"-2rem"
            },200)
        });
    });

    var changeHeight = function(i){
        $(".upload").css("height", 1.2 * i + "rem");
    };

    //to binary
    function dataURItoBlob(base64Data) {
        var byteString;
        if (base64Data.split(',')[0].indexOf('base64') >= 0)
            byteString = atob(base64Data.split(',')[1]);
        else
            byteString = unescape(base64Data.split(',')[1]);
        var mimeString = base64Data.split(',')[0].split(':')[1].split(';')[0];
        var ia = new Uint8Array(byteString.length);
        for (var i = 0; i < byteString.length; i++) {
            ia[i] = byteString.charCodeAt(i);
        }
        return new Blob([ia], {type:mimeString});
    }

    var tusitemp="";
    function mess_tusi(strs){
        //清除事件
        clearTimeout(tusitemp);
        $("#mess_tusi").remove();
        //创建吐丝层并写入内容
        if(!$("#mess_tusi").attr("id")){ //吐丝层不存在创建
            $("body").append("<div id='mess_tusi' style='z-index: 100002;position:fixed;font-size:16px;border-radius:4px !important;background:rgba(0,0,0,.7);color:#fff;display:none;'><span style='display:block;padding:5px 15px;'>"+strs+"</span></div>"); //写入内容
        }else{
            $("#mess_tusi").html(strs);  //写入内容
        }

        //定义吐丝层位置
        var left=($(window).width()-$("#mess_tusi").width())/2;//居中
        //var top=($(window).height()-$("#mess_tusi").height())/2;//居中
        var top=$(window).height()*0.45;//偏下
        $("#mess_tusi").css({"left":left+"px","top":top+"px"});

        //显示吐丝层
        $("#mess_tusi").css("display",'');

        //2秒后关闭
        tusitemp =  setTimeout(function (){
            $("#mess_tusi").remove();
            $("#mess_tusi").html("");
        },1000);
        return false;
    }

    var prog = function(n){
        $(".progress").show();
        $(".active-pro").show();
        $(".text").show();
        $(".active-pro").css("width", n * 3 + "px");
        $(".text").text(n + "%");
    };

    //上传
    var imgAddress;
    var upload = function(url, formData, src, id){

        var loaded = false;

        setTimeout(function(){
            mess_tusi("正在上传中，请稍后...");
        }, 500);

        var loading = setInterval(function(){
            if (!loaded){
                mess_tusi("正在上传中，请稍后...");
            } else {
                clearInterval(loading);
            }
        }, 1000);

        /*var xhr = new XMLHttpRequest();
        /!* event listners *!/
        xhr.upload.addEventListener("progress", uploadProgress, false);
        xhr.addEventListener("load", uploadComplete, false);
        xhr.addEventListener("error", uploadFailed, false);
        xhr.addEventListener("abort", uploadCanceled, false);
        xhr.open("POST", "http://localhost:8400/upload");
        //xhr.open("POST", "https://changangw.mouchina.com/common/resource/uploadPictures");
        xhr.send(formData);

        function uploadProgress(evt) {
            if (evt.lengthComputable) {
                var percentComplete = Math.round(evt.loaded * 100 / evt.total);
                prog(percentComplete);
                //document.getElementById('progressNumber').innerHTML = percentComplete.toString() + '%';
            }
            else {
                //document.getElementById('progressNumber').innerHTML = 'unable to compute';
            }
        }

        function uploadComplete(evt) {
            /!* This event is raised when the server send back a response *!/
            console.log(evt.target.responseText);

        }

        function uploadFailed(evt) {
            console.log("There was an error attempting to upload the file.");
        }

        function uploadCanceled(evt) {
            console.log("The upload has been canceled by the user or the browser dropped the connection.");
        }*/

        $.ajax({
            url: url,
            type: 'POST',
            data: formData,
            cache: false,
            contentType: false,
            processData: false,
            success:function(data){
                //console.log(data.errorCode);
                if(data.errorCode == 100101 || data.errorCode== 100001){
                    loaded = true;
                    mess_tusi("用户已在别处失败，请重新登录");
                    setTimeout(function(){
                        localStorage.removeItem("phoneNum");
                        localStorage.removeItem("password");
                        window.location.href="login.html";
                    }, 1000);
                } else
                if (url.match(/\/upload\w+/g)[0].substring(7) === "Pictures"){ //区分uploadVideos
                    //if (url.substring(0, 5) === "https"){ //区分upload和Pictures
                    imgAddress = data.photos;
                    var width = getInfo(src).match(/\d+/g)[0];
                    var height = getInfo(src).match(/\d+/g)[1];
                    imgInfo.photo.push({"url": imgAddress[0], "width": width, "height": height, "id": id});
                    //console.log(imgInfo);
                    //console.log(imgData);
                    console.log("success: " + imgAddress);
                    $(".mask").hide();
                    $(".approve").stop().animate({
                        "bottom":"-2rem"
                    },200);
                    mess_tusi("图片上传成功");
                    loaded = true;
                } else {
                    //videoLink = data;
                    videoLink = data.videos[0];
                    console.log(videoLink);
                    $(".mask").hide();
                    $(".approve-1").stop().animate({
                        "bottom":"-2rem"
                    },200);
                    mess_tusi("视频上传成功");
                    $(".show-video").remove();
                    $(".upload-video").append("<video id='vd' src=" + videoLink + " controls='controls'></video>");
                    $("#vd").css({
                        "position": "relative",
                        "width": "0.77rem",
                        "height": "0.77rem",
                        "margin-left": "0.3rem",
                        "border": "dashed 1px #e6e6e6",
                        "border-radius": "0.1rem",
                        "float": "left"
                    });
                    loaded = true;
                    //console.log(data.videos);
                }
            },
            error:function(xhr){
                console.log("error: " + xhr);
            }
        });

    };

    //获取图片宽高信息
    var getInfo = function(img){
        var image = new Image();
        image.src = img;
        return image.width + "/" + image.height;
    };

    //删除图片
    $(".upload").on('tap', function(e){
        e.stopPropagation();
        var id = $(e.target).attr("id");
        if (id.match(/^close/g)){
            var pid = id.substring(5);
            //console.log(pid);
            imgInfo = delInfo(imgInfo, pid);
            imgData = delData(imgData, pid);
            //console.log(imgInfo);
            //console.log(imgData);
            $("#show" + pid).remove();
            if (count % 3 == 0){
                line -= 1;
                changeHeight(line);
            }
            count = count - 1;
            console.log("del count: " + count);
            if (!showAlive){
                $(".upload").append("<div class='show' id='show'></div>");
                console.log("add show");
                addPic();
                showAlive = true;
            }
        }
    });

    //size
    var size = function(blob){
        var fileSize = 0;
        if (blob.size > 1024 * 1024) {
            fileSize = (Math.round(blob.size * 100 / (1024 * 1024)) / 100).toString() + 'MB';
            console.log(fileSize);
        }
        else {
            fileSize = (Math.round(blob.size * 100 / 1024) / 100).toString() + 'KB';
            console.log(fileSize);
        }
    };

    //压缩图片
    var compPic;
    var compress = function(){
        $('.image-upload-0').imageCompress({
            'quality': 30,
            'onloadStart': function(result){
                //console.log('读取图片开始'+result);
            },
            'onloadEnd': function(result){
                //console.log('读取图片结束'+result);
            },
            'oncompressStart': function(result){
                //console.log('压缩图片开始'+result);
            },
            'oncompressEnd': function(result){
                //console.log('压缩图片结束'+result);
                $('#preview').append(result);
                //$('#preview').find('img').addClass('preview');
                compPic = $('#preview').find('img').attr("src");
                //console.log(compPic);
            },
            'callback': function(){
                console.log('处理完毕');
            }
        });
    };

    compress();

    var file_0 = "";
    var sum = 0;
    $(".opprove-box").change(function () {
        var check = setInterval(function(){
            if (compPic != undefined){
                clearInterval(check);
                var formData_0 = new FormData($("#formdata-0")[0]);
                var len = document.getElementById("file-0").files.length;
                sum += len;

                if (sum <= 9){
                    for (var i = 0; i < len; i++){
                        var filename = "filename" + i;
                        file_0 = document.getElementById("file-0").files[i];
                        var blob = dataURItoBlob(compPic);
                        size(blob);
                        //var fd = new FormData(document.forms[0]);
                        //upload(imgUrl, fd, compPic, id);

                        var fd = new FormData();
                        //console.log(document.forms[0]);
                        fd.append("name", "files");
                        fd.append("filename", filename);
                        fd.append("files", blob);
                        upload(imgUrl, fd, compPic, id);
                        //upload(url, fd, compPic, id);
                        imgData.push({id: id, data: formData_0});

                        evt_id = "show" + id;
                        close_id = "close" + id;
                        id = id + 1;
                        //console.log("evt_id=" + evt_id + " close_id=" + close_id);
                        $(".upload").append("<div class='show' id=" + evt_id + "><div class='close' id=" + close_id + "></div></div>");
                        $(".upload").append("<div class='show' id='show'></div>");
                        $("#show").remove();

                        count = count + 1;
                        console.log("add count:" + count);
                        if (count <= 9){
                            if (count == 9) {
                                line += 1;
                                $("#show").remove();
                                showAlive = false;
                            }
                            if (count != 0 && count!= 9 && count % 3 == 0){
                                line += 1;
                                console.log("line: " + line);
                                changeHeight(line);
                            }
                            $("#" + evt_id).css({"background": "url(" + compPic + ") no-repeat center",
                                "background-size": "100%"});
                            addPic();
                        } else {
                            line += 1;
                            $("#show").remove();
                            console.log("count over");
                            showAlive = false;
                        }
                    }
                }
            }
        }, 100);
        $('#preview img').remove();
    });

    //upload video
    /*function notifyVideoPath(path){
        $("#res").text(path);
        var blob = dataURItoBlob(path);
        var fd = new FormData(document.forms[1]);
        fd.append("file", blob);
        upload(videoUrl, fd);
        //upload(url, fd);
        size(blob);
    }*/

    /*function notifyVideoPath(path){
        //console.log(path);
        var file_1 = document.getElementById("file-1").files[0];
        console.log(file_1);
        var res = document.getElementById('res');
        res.innerHTML = path;
        var fd = new FormData(path);
        fd.append("file", path);
        upload(videoUrl, fd);

        //console.log(fd);

/!*        if(window.FileReader) {
            console.log("FileReader");
            var reader = new FileReader();
            reader.onload = function (e) {
//                    var blob = dataURItoBlob(this.result); // 上一步中的函数
                console.log(this.result);

                //var fd = new FormData(document.forms[1]);
                //console.log(fd);
//                    fd.append("file", blob);
                //console.log(this.result);
                //console.log(file_1.size);
                //console.log(fd);
//                    upload(videoUrl, fd);
                //upload(url, fd);
            };
            reader.readAsDataURL(path);
        }*!/
    }*/

    //notifyVideoPath('e:/VID_20170425_174700.mp4');

    /*$(".video").change(function(){
        //var len =  document.getElementById("file-1").files.length;
        //console.log(len);
        //for (var i = 0; i < len; i++){
        //notifyVideoPath('');
        //return;
            var file_1 = document.getElementById("file-1").files[0];
            console.log(file_1);
            var reader = new FileReader();
            reader.readAsDataURL(file_1);
            reader.onloadend=function(e) {
                var blob = dataURItoBlob(this.result); // 上一步中的函数
                //console.log(blob.size);
                //console.log(blob.type);
                var fd = new FormData(document.forms[1]);
                fd.append("file", blob);
                //console.log(this.result);
                //console.log(file_1.size);
                //console.log(fd);
                upload(videoUrl, fd);
                //upload(url, fd);

                size(blob);

                //var canvas = document.createElement('canvas');
                //var dataURL = canvas.toDataURL('video/mp4', 0.5);
            };
        //}
    });*/

    window.init = function() {
        var map, geolocation;
        //加载地图，调用浏览器定位服务
        map = new AMap.Map('container', {
            resizeEnable: true
        });
        map.plugin('AMap.Geolocation', function () {
            geolocation = new AMap.Geolocation({
                enableHighAccuracy: true,//是否使用高精度定位，默认:true
                timeout: 10000,          //超过10秒后停止定位，默认：无穷大
                buttonOffset: new AMap.Pixel(10, 20),//定位按钮与设置的停靠位置的偏移量，默认：Pixel(10, 20)
                zoomToAccuracy: true,      //定位成功后调整地图视野范围使定位位置及精度范围视野内可见，默认：false
                buttonPosition: 'RB'
            });
            map.addControl(geolocation);
            geolocation.getCurrentPosition();
            AMap.event.addListener(geolocation, 'complete', onComplete);//返回定位信息
            AMap.event.addListener(geolocation, 'error', onError);      //返回定位出错信息
        });
        //解析定位结果
        function onComplete(data) {
            longitude = data.position.getLng();
            latitude = data.position.getLat();
            location = [longitude, latitude];
            console.log(location);
        }
        //解析定位错误信息
        function onError(data) {
            document.getElementById('tip').innerHTML = '定位失败';
        }
    };

    $(".button").click(function(){
        var title = $("#title").val();
        var content = $("#content").val();

        if (title == "" || content == ""){
            mess_tusi("请完善信息后提交");
            return;
        }

        var jsonData = {
            loginKey: loginKey,
            type: titleType,
            longitude: longitude,
            latitude: latitude,
            reportTitle: title,
            reportContent: content,
            reportImageUrl: JSON.stringify(imgInfo),
            reportVideoUrl: videoLink,
            reportAudioUrl: "",
            anonymousReport: 0
            };

        $.ajax({
            url: submit + "?loginKey=" + loginKey,
            type: 'GET',
            data: jsonData,
            success: function(data){
                console.log(data);
                mess_tusi("举报信息上传成功");
                setTimeout(function(){
                    if(data.errorCode == 100101 || data.errCode == 100101){
                        $(".hides").show();
                        $(".cance").click(function(){
                            localStorage.removeItem("phoneNum");
                            localStorage.removeItem("password");
                            window.location.href="login.html";
                        })
                    }else if(data.result==1 || data.result=="1"){
                        window.location.href="../index/changan-my.html?loginKey="+loginKey+"&userId="+userId+"&userType="+userType+"&type="+titleType;
                    }
                }, 500);
            },
            error: function(xhr){
                console.log(xhr);
            }
        });
    });

    /*一键拨号110*/
    $("#kefu").click(function(){
        $(".moddle").show();
    });
    $(".cancel").click(function(){
        $(".moddle").hide();
    });
});

