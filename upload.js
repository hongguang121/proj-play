    
    var data_url = "";
    var formData = new FormData($("#formdata")[0]);

            $.ajax({
                    url: "/common/resource/image/upload.json",                
                    type: 'POST',
                    data: formData,
                    async: true,
                    cache: false,
                    contentType: false,
                    processData: false,
                    success: function (data) {
                        //上传成功并获得图片地址
                        data_url = JSON.parse(data).image.url;                       

                        var file = document.getElementById("file").files[0];
                        var reader = new FileReader();
                        reader.readAsDataURL(file);

                        reader.onload=function(e) {
                            var result = document.getElementById("file");
                            //result是图片本身，可以在不上传的状态下显示出来
                            
                        };

                    
                    },
                    error: function (data) {
                        console.log("error:" + data);
                    }
                });