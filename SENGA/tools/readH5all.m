function  readH5all(filename)
    fileInfo = h5info(filename);

    for k = 1:numel(fileInfo.Groups)
        dsetName = fileInfo.Groups(k).Name;
        fullPath_dataset = [fileInfo.Name, dsetName];
        
        for i= 1:numel(fileInfo.Groups(k).Datasets)
            nameData= fileInfo.Groups(k).Datasets(i).Name;
            fullPath_variable=[fullPath_dataset, '/', nameData]
            varName = matlab.lang.makeValidName(nameData);
            data = h5read(filename, fullPath_variable);    
            eval([varName ' = data;']);
        end
    end

       clear data dsetName fullPath_variable fullPath_dataset i k nameData  varName
       filename = char(filename);
       output_filename = [filename(1:end-3) , '.mat'];
       save(output_filename,'-v7.3')
       disp(['100% done; The file ' filename  ' is completely read and the output is saved in the file ' output_filename])


end
